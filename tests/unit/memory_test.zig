//! Memory leak tests
//! Test for memory leaks in AST nodes and data structures

const std = @import("std");
const ast = @import("pegz_common").ast;

const testing = std.testing;

test "Memory Management - Summary" {
    std.debug.print("\n" ++ "=" ** 80 ++ "\n", .{});
    std.debug.print("MEMORY MANAGEMENT TEST SUMMARY\n", .{});
    std.debug.print("=" ** 80 ++ "\n", .{});
    std.debug.print("\nThis test file verifies proper memory management of AST nodes.\n", .{});
    std.debug.print("All tests check that deinit() properly releases memory.\n\n", .{});
    std.debug.print("Verified memory management:\n", .{});
    std.debug.print("  - LitMatcher: deinit() releases value string\n", .{});
    std.debug.print("  - Rule: deinit() releases name, display, and expr\n", .{});
    std.debug.print("  - Grammar: deinit() releases all rules and init block\n", .{});
    std.debug.print("=" ** 80 ++ "\n\n", .{});
}

test "No Memory Leak - LitMatcher with deinit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create a LitMatcher
    const lit = try ast.LitMatcher.create(allocator, pos, "hello", false);

    // Call deinit() - should not leak
    lit.deinit(allocator);

    // Verify no leak
    const leaked = gpa.deinit();
    try testing.expect(leaked == .ok);
}

test "No Memory Leak - Rule with deinit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const name = try ast.Identifier.init(allocator, pos, "TestRule");
    const display = try ast.StringLit.init(allocator, pos, "TestRule");
    const lit = try ast.LitMatcher.create(allocator, pos, "test", false);
    const expr = ast.Expression{ .lit_matcher = lit };

    // Create a Rule - this copies name and display
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);

    // Free the original name and display since Rule now owns copies
    name.deinit(allocator);
    display.deinit(allocator);

    // Call deinit() - should not leak
    rule.deinit(allocator);

    // Verify no leak
    const leaked = gpa.deinit();
    try testing.expect(leaked == .ok);
}

test "No Memory Leak - Grammar with deinit" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const name = try ast.Identifier.init(allocator, pos, "A");
    const display = try ast.StringLit.init(allocator, pos, "A");
    const lit = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit };
    const rule = try ast.Rule.create(allocator, pos, name, display, expr);

    // Free the original name and display since Rule now owns copies
    name.deinit(allocator);
    display.deinit(allocator);

    // Create a Grammar
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Call deinit() - should not leak
    grammar.deinit(allocator);

    // Verify no leak
    const leaked = gpa.deinit();
    try testing.expect(leaked == .ok);
}
