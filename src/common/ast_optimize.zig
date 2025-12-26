//! AST optimization
//! Based on pigeon's ast/ast_optimize.go

const std = @import("std");
const ast = @import("ast.zig");

/// Optimize a grammar by applying all optimizations
pub fn optimizeGrammar(allocator: std.mem.Allocator, grammar: *ast.Grammar) !*ast.Grammar {
    // Create new optimized grammar
    var optimized_rules = try std.ArrayList(*ast.Rule).initCapacity(allocator, grammar.rules.items.len);

    for (grammar.rules.items) |rule| {
        // Step 1: Optimize expression structure
        var optimized_expr = try optimizeExpr(allocator, &rule.expr);

        // Step 2: Simplify expressions (remove duplicates, etc.)
        optimized_expr = try simplifyExpr(allocator, &optimized_expr);

        // Step 3: Fold constants
        optimized_expr = try foldConstants(allocator, &optimized_expr);

        const optimized_rule = try ast.Rule.create(
            allocator,
            rule.pos,
            rule.name,
            rule.display_name,
            optimized_expr,
        );
        optimized_rule.visited = rule.visited;
        optimized_rule.nullable = rule.nullable;
        optimized_rule.left_recursive = rule.left_recursive;
        optimized_rule.leader = rule.leader;
        try optimized_rules.append(allocator, optimized_rule);
    }

    return ast.Grammar.create(allocator, grammar.pos, optimized_rules.items) catch |err| {
        // Clean up on error
        for (optimized_rules.items) |r| {
            allocator.destroy(r);
        }
        return err;
    };
}

/// Optimize an expression
fn optimizeExpr(allocator: std.mem.Allocator, expr: *const ast.Expression) anyerror!ast.Expression {
    return switch (expr.*) {
        .seq => |seq| blk: {
            const optimized = try optimizeSeq(allocator, seq);
            break :blk ast.Expression{ .seq = optimized };
        },
        .choice => |choice| blk: {
            const optimized = try optimizeChoice(allocator, choice);
            break :blk ast.Expression{ .choice = optimized };
        },
        .zero_or_one => |z| blk: {
            const optimized = try optimizeZeroOrOne(allocator, z);
            break :blk ast.Expression{ .zero_or_one = optimized };
        },
        .zero_or_more => |z| blk: {
            const optimized = try optimizeZeroOrMore(allocator, z);
            break :blk ast.Expression{ .zero_or_more = optimized };
        },
        .one_or_more => |o| blk: {
            const optimized = try optimizeOneOrMore(allocator, o);
            break :blk ast.Expression{ .one_or_more = optimized };
        },
        // For other expression types, just return as-is for now
        else => expr.*,
    };
}

/// Optimize sequence expression
fn optimizeSeq(allocator: std.mem.Allocator, seq: *const ast.SeqExpr) !*ast.SeqExpr {
    var optimized_exprs = try std.ArrayList(ast.Expression).initCapacity(allocator, seq.exprs.items.len);

    var changed = false;
    for (seq.exprs.items) |expr| {
        const optimized = try optimizeExpr(allocator, &expr);
        try optimized_exprs.append(allocator, optimized);
        if (!std.meta.eql(expr, optimized)) {
            changed = true;
        }
    }

    // If no changes, return original
    if (!changed) {
        return @constCast(seq);
    }

    // Create new SeqExpr
    return ast.SeqExpr.create(allocator, seq.pos, optimized_exprs.items);
}

/// Optimize choice expression - merge duplicate branches
fn optimizeChoice(allocator: std.mem.Allocator, choice: *const ast.ChoiceExpr) !*ast.ChoiceExpr {
    var optimized_alternatives = try std.ArrayList(ast.Expression).initCapacity(allocator, choice.alternatives.items.len);

    for (choice.alternatives.items) |expr| {
        const optimized = try optimizeExpr(allocator, &expr);
        // TODO: detect and remove duplicates
        try optimized_alternatives.append(allocator, optimized);
    }

    return ast.ChoiceExpr.create(allocator, choice.pos, optimized_alternatives.items);
}

/// Optimize zero-or-one expression
fn optimizeZeroOrOne(allocator: std.mem.Allocator, z: *const ast.ZeroOrOneExpr) !*ast.ZeroOrOneExpr {
    const optimized = try optimizeExpr(allocator, &z.expr);
    return ast.ZeroOrOneExpr.create(allocator, z.pos, optimized);
}

/// Optimize zero-or-more expression
fn optimizeZeroOrMore(allocator: std.mem.Allocator, z: *const ast.ZeroOrMoreExpr) !*ast.ZeroOrMoreExpr {
    const optimized = try optimizeExpr(allocator, &z.expr);
    return ast.ZeroOrMoreExpr.create(allocator, z.pos, optimized);
}

/// Optimize one-or-more expression
fn optimizeOneOrMore(allocator: std.mem.Allocator, o: *const ast.OneOrMoreExpr) !*ast.OneOrMoreExpr {
    const optimized = try optimizeExpr(allocator, &o.expr);
    return ast.OneOrMoreExpr.create(allocator, o.pos, optimized);
}

/// Simplify an expression by removing redundant constructs
pub fn simplifyExpr(allocator: std.mem.Allocator, expr: *const ast.Expression) !ast.Expression {
    return switch (expr.*) {
        .choice => |choice| blk: {
            // Remove duplicate branches
            var seen = std.StringHashMap(void).init(allocator);
            defer seen.deinit();
            var unique = try std.ArrayList(ast.Expression).initCapacity(allocator, choice.alternatives.items.len);

            for (choice.alternatives.items) |alt| {
                const key = try exprToString(allocator, alt);
                if (!seen.contains(key)) {
                    try seen.put(key, {});
                    try unique.append(allocator, alt);
                }
                allocator.free(key);
            }

            if (unique.items.len == 1) {
                // Single branch - return it directly
                return unique.items[0];
            }

            if (unique.items.len < choice.alternatives.items.len) {
                break :blk ast.Expression{ .choice = try ast.ChoiceExpr.create(allocator, choice.pos, unique.items) };
            } else {
                break :blk expr.*;
            }
        },
        else => expr.*,
    };
}

/// Fold constant expressions
pub fn foldConstants(allocator: std.mem.Allocator, expr: *const ast.Expression) !ast.Expression {
    return switch (expr.*) {
        // Fold sequences of literals: "a" "b" "c" => "abc"
        .seq => |seq| blk: {
            var can_merge = true;
            var total_len: usize = 0;

            // Check if all elements are literal matchers
            for (seq.exprs.items) |e| {
                if (e != .lit_matcher) {
                    can_merge = false;
                    break;
                }
                total_len += e.lit_matcher.value.len;
            }

            if (can_merge and seq.exprs.items.len > 1) {
                // Merge all literals into one
                var merged = try std.ArrayList(u8).initCapacity(allocator, total_len);
                for (seq.exprs.items) |e| {
                    try merged.appendSlice(allocator, e.lit_matcher.value);
                }

                const merged_lit = try ast.LitMatcher.create(allocator, seq.pos, merged.items, false);
                break :blk ast.Expression{ .lit_matcher = merged_lit };
            } else {
                // Recursively fold child expressions
                var folded_exprs = try std.ArrayList(ast.Expression).initCapacity(allocator, seq.exprs.items.len);
                for (seq.exprs.items) |e| {
                    const folded = try foldConstants(allocator, &e);
                    try folded_exprs.append(allocator, folded);
                }
                const folded_seq = try ast.SeqExpr.create(allocator, seq.pos, folded_exprs.items);
                break :blk ast.Expression{ .seq = folded_seq };
            }
        },

        // Fold choices with single branch: (a / b / c) where all are same => a
        .choice => |choice| blk: {
            // Recursively fold all alternatives
            var folded_alts = try std.ArrayList(ast.Expression).initCapacity(allocator, choice.alternatives.items.len);
            for (choice.alternatives.items) |alt| {
                const folded = try foldConstants(allocator, &alt);
                try folded_alts.append(allocator, folded);
            }

            // If all alternatives are the same literal, return one
            if (folded_alts.items.len > 0) {
                const first = folded_alts.items[0];
                var all_same = true;
                for (folded_alts.items[1..]) |alt| {
                    if (!std.meta.eql(first, alt)) {
                        all_same = false;
                        break;
                    }
                }

                if (all_same) {
                    break :blk first;
                }
            }

            const folded_choice = try ast.ChoiceExpr.create(allocator, choice.pos, folded_alts.items);
            break :blk ast.Expression{ .choice = folded_choice };
        },

        // Simplify nested repetition: (a*)* => a*, (a+)* => a+
        .zero_or_more => |zom| blk: {
            const folded = try foldConstants(allocator, &zom.expr);
            // If inner expression is also zero_or_more, return inner
            if (folded == .zero_or_more) {
                break :blk folded;
            }
            const new_zom = try ast.ZeroOrMoreExpr.create(allocator, zom.pos, folded);
            break :blk ast.Expression{ .zero_or_more = new_zom };
        },

        // Simplify nested one_or_more: (a+)+ => a+
        .one_or_more => |oom| blk: {
            const folded = try foldConstants(allocator, &oom.expr);
            // If inner expression is also one_or_more, return inner
            if (folded == .one_or_more) {
                break :blk folded;
            }
            const new_oom = try ast.OneOrMoreExpr.create(allocator, oom.pos, folded);
            break :blk ast.Expression{ .one_or_more = new_oom };
        },

        // Simplify optional of optional: (a?)? => a?
        .zero_or_one => |zoo| blk: {
            const folded = try foldConstants(allocator, &zoo.expr);
            // If inner expression is also zero_or_one, return inner
            if (folded == .zero_or_one) {
                break :blk folded;
            }
            const new_zoo = try ast.ZeroOrOneExpr.create(allocator, zoo.pos, folded);
            break :blk ast.Expression{ .zero_or_one = new_zoo };
        },

        // For other expressions, just return as-is
        else => expr.*,
    };
}

/// Helper function to convert expression to string (for deduplication)
fn exprToString(allocator: std.mem.Allocator, expr: ast.Expression) ![]const u8 {
    var buffer = try std.ArrayList(u8).initCapacity(allocator, 32);
    const writer = buffer.writer(allocator);

    switch (expr) {
        .lit_matcher => |lit| {
            try writer.print("\"{s}\"", .{lit.value});
        },
        .char_class_matcher => {
            try writer.writeAll("[");
            // TODO: write full class representation
            try writer.writeAll("]");
        },
        .rule_ref => |ref| {
            try writer.print("{s}", .{ref.name.value});
        },
        else => {
            try writer.writeAll("?");
        },
    }

    return buffer.toOwnedSlice(allocator);
}
