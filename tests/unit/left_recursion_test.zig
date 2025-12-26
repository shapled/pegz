//! Unit tests for Left Recursion detection
//! Task 6.2: 左递归检测测试

const std = @import("std");
const ast = @import("pegz_common").ast;
const left_rec_mod = @import("pegz_common").left_recursion;
const computeNullables = left_rec_mod.computeNullables;
const makeFirstGraph = left_rec_mod.makeFirstGraph;
const computeLeftRecursives = left_rec_mod.computeLeftRecursives;
const prepareGrammar = left_rec_mod.prepareGrammar;

const testing = std.testing;

test "Left Recursion - direct left recursion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create grammar: Expr <- Expr "+" Term
    const name = ast.Identifier.init(pos, "Expr");
    const display = ast.StringLit.init(pos, "Expr");

    // Simple expression (just reference to Expr for left recursion)
    const ref_name = ast.Identifier.init(pos, "Expr");
    const expr_ref = try ast.RuleRefExpr.create(allocator, pos, ref_name);
    const expr = ast.Expression{ .rule_ref = expr_ref };

    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Prepare grammar
    const result = try prepareGrammar(allocator, grammar);

    try testing.expect(result.have_left_recursion);
    try testing.expect(rule.left_recursive);
    try testing.expect(rule.leader);
}

test "Left Recursion - indirect left recursion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create grammar: A <- B, B <- A
    const name_a = ast.Identifier.init(pos, "A");
    const display_a = ast.StringLit.init(pos, "A");
    const ref_b = ast.Identifier.init(pos, "B");
    const expr_b = try ast.RuleRefExpr.create(allocator, pos, ref_b);
    const rule_a = try ast.Rule.create(allocator, pos, name_a, display_a, ast.Expression{ .rule_ref = expr_b });

    const name_b = ast.Identifier.init(pos, "B");
    const display_b = ast.StringLit.init(pos, "B");
    const ref_a = ast.Identifier.init(pos, "A");
    const expr_a = try ast.RuleRefExpr.create(allocator, pos, ref_a);
    const rule_b = try ast.Rule.create(allocator, pos, name_b, display_b, ast.Expression{ .rule_ref = expr_a });

    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{ rule_a, rule_b });

    // Prepare grammar
    const result = try prepareGrammar(allocator, grammar);

    try testing.expect(result.have_left_recursion);
    // Both A and B should be marked as left recursive
    try testing.expect(rule_a.left_recursive);
    try testing.expect(rule_b.left_recursive);
    // One of them should be leader
    try testing.expect(rule_a.leader or rule_b.leader);
}

test "Left Recursion - no recursion" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create grammar: A <- "hello"
    const name = ast.Identifier.init(pos, "A");
    const display = ast.StringLit.init(pos, "A");
    const lit = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit };
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Prepare grammar
    const result = try prepareGrammar(allocator, grammar);

    try testing.expect(!result.have_left_recursion);
    try testing.expect(!rule.left_recursive);
    try testing.expect(!rule.leader);
}

test "Left Recursion - mixed rules" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create grammar with mixed recursive and non-recursive rules
    // Expr <- Expr "+" Term  (left recursive)
    // Term <- "number"       (not recursive)

    const name_expr = ast.Identifier.init(pos, "Expr");
    const display_expr = ast.StringLit.init(pos, "Expr");
    const ref_expr = ast.Identifier.init(pos, "Expr");
    const expr_ref = try ast.RuleRefExpr.create(allocator, pos, ref_expr);
    const expr_expr = ast.Expression{ .rule_ref = expr_ref };
    const rule_expr = try ast.Rule.create(allocator, pos, name_expr, display_expr, expr_expr);

    const name_term = ast.Identifier.init(pos, "Term");
    const display_term = ast.StringLit.init(pos, "Term");
    const lit = try ast.LitMatcher.create(allocator, pos, "number", false);
    const expr_term = ast.Expression{ .lit_matcher = lit };
    const rule_term = try ast.Rule.create(allocator, pos, name_term, display_term, expr_term);

    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{ rule_expr, rule_term });

    // Prepare grammar
    const result = try prepareGrammar(allocator, grammar);

    try testing.expect(result.have_left_recursion);
    try testing.expect(rule_expr.left_recursive);
    try testing.expect(!rule_term.left_recursive);
}
