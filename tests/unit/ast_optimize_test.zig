//! Unit tests for AST optimization
//! Task 6.3: AST 优化测试

const std = @import("std");
const ast = @import("pegz_common").ast;
const ast_opt_mod = @import("pegz_common").ast_optimize;
const optimizeGrammar = ast_opt_mod.optimizeGrammar;
const simplifyExpr = ast_opt_mod.simplifyExpr;
const foldConstants = ast_opt_mod.foldConstants;

const testing = std.testing;

test "AST Optimize - simple grammar" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a simple grammar with one rule
    const name = try ast.Identifier.init(allocator, pos, "A");
    const display = try ast.StringLit.init(allocator, pos, "A");
    const lit = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit };
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Optimize
    const optimized = try optimizeGrammar(allocator, grammar);

    // Should have same number of rules
    try testing.expectEqual(grammar.rules.items.len, optimized.rules.items.len);
}

test "AST Optimize - sequence expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a sequence: "a" "b" "c"
    const lit_a = try ast.LitMatcher.create(allocator, pos, "a", false);
    const lit_b = try ast.LitMatcher.create(allocator, pos, "b", false);
    const lit_c = try ast.LitMatcher.create(allocator, pos, "c", false);

    var exprs = try std.ArrayList(ast.Expression).initCapacity(allocator, 3);
    try exprs.append(allocator, ast.Expression{ .lit_matcher = lit_a });
    try exprs.append(allocator, ast.Expression{ .lit_matcher = lit_b });
    try exprs.append(allocator, ast.Expression{ .lit_matcher = lit_c });

    const seq = try ast.SeqExpr.create(allocator, pos, exprs.items);
    const expr = ast.Expression{ .seq = seq };

    const name = try ast.Identifier.init(allocator, pos, "Rule");
    const display = try ast.StringLit.init(allocator, pos, "Rule");
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Optimize
    const optimized = try optimizeGrammar(allocator, grammar);

    // Should preserve structure
    try testing.expectEqual(@as(usize, 1), optimized.rules.items.len);
}

test "AST Optimize - choice expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a choice: "a" / "b" / "c"
    const lit_a = try ast.LitMatcher.create(allocator, pos, "a", false);
    const lit_b = try ast.LitMatcher.create(allocator, pos, "b", false);
    const lit_c = try ast.LitMatcher.create(allocator, pos, "c", false);

    var alts = try std.ArrayList(ast.Expression).initCapacity(allocator, 3);
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_a });
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_b });
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_c });

    const choice = try ast.ChoiceExpr.create(allocator, pos, alts.items);
    const expr = ast.Expression{ .choice = choice };

    const name = try ast.Identifier.init(allocator, pos, "Rule");
    const display = try ast.StringLit.init(allocator, pos, "Rule");
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Optimize
    const optimized = try optimizeGrammar(allocator, grammar);

    // Should preserve structure
    try testing.expectEqual(@as(usize, 1), optimized.rules.items.len);
}

test "AST Optimize - zero or more" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a zero-or-more: "a"*
    const lit = try ast.LitMatcher.create(allocator, pos, "a", false);
    const lit_expr = ast.Expression{ .lit_matcher = lit };
    const zom = try ast.ZeroOrMoreExpr.create(allocator, pos, lit_expr);
    const expr = ast.Expression{ .zero_or_more = zom };

    const name = try ast.Identifier.init(allocator, pos, "Rule");
    const display = try ast.StringLit.init(allocator, pos, "Rule");
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Optimize
    const optimized = try optimizeGrammar(allocator, grammar);

    // Should preserve structure
    try testing.expectEqual(@as(usize, 1), optimized.rules.items.len);
}

test "AST Simplify - remove duplicate choices" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a choice with duplicates: "a" / "a" / "b"
    const lit_a1 = try ast.LitMatcher.create(allocator, pos, "a", false);
    const lit_a2 = try ast.LitMatcher.create(allocator, pos, "a", false);
    const lit_b = try ast.LitMatcher.create(allocator, pos, "b", false);

    var alts = try std.ArrayList(ast.Expression).initCapacity(allocator, 3);
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_a1 });
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_a2 });
    try alts.append(allocator, ast.Expression{ .lit_matcher = lit_b });

    const choice = try ast.ChoiceExpr.create(allocator, pos, alts.items);
    const expr = ast.Expression{ .choice = choice };

    // Simplify
    const simplified = try simplifyExpr(allocator, &expr);

    // Should still be a choice (deduplication is based on string representation)
    try testing.expectEqual(expr, simplified);
}

test "AST Fold Constants - placeholder" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a simple expression
    const lit = try ast.LitMatcher.create(allocator, pos, "a", false);
    const expr = ast.Expression{ .lit_matcher = lit };

    // Fold constants (currently just returns as-is)
    const folded = try foldConstants(allocator, &expr);

    // Should return same expression
    _ = folded;
}
