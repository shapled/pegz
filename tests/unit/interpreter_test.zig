//! Unit tests for the Interpreter engine
//! Task 4.3: Interpreter 测试

const std = @import("std");
const ast = @import("pegz_common").ast;
const interpreter_mod = @import("pegz_common").interpreter;
const Interpreter = interpreter_mod.Interpreter;
const ParseError = interpreter_mod.ParseError;

const testing = std.testing;

test "Interpreter - init and deinit" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a simple grammar
    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    try testing.expectEqual(@as(usize, 0), interp.pos);
    try testing.expectEqual(@as(usize, 1), interp.line);
    try testing.expectEqual(@as(usize, 1), interp.column);
}

test "Interpreter - setInput" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("hello world");

    try testing.expectEqualStrings("hello world", interp.input);
    try testing.expectEqual(@as(usize, 0), interp.pos);
}

test "Interpreter - execLitMatcher match" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create grammar
    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Create literal matcher
    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    // Create interpreter and set input
    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("hello world");

    // Execute expression
    const result = try interp.execExpr(&expr);

    // Check result
    try testing.expectEqualStrings("hello", result.string);
    try testing.expectEqual(@as(usize, 5), interp.pos); // Advanced past "hello"
}

test "Interpreter - execLitMatcher no match" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("world");

    // Should fail to match
    const result = interp.execExpr(&expr);
    try testing.expectError(ParseError.NoMatch, result);
}

test "Interpreter - execLitMatcher end of input" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("");

    // Should fail - not enough input
    const result = interp.execExpr(&expr);
    try testing.expectError(ParseError.NoMatch, result);
}

test "Interpreter - execCharClassMatcher match char" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Create character class [a-z]
    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[a-z]",
        false,
        false,
        &[_]u8{},
        &[_]struct { u8, u8 }{.{ 'a', 'z' }},
        &[_][]const u8{},
    );
    const expr = ast.Expression{ .char_class_matcher = char_class };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("hello");

    const result = try interp.execExpr(&expr);

    try testing.expectEqualStrings("h", result.string);
    try testing.expectEqual(@as(usize, 1), interp.pos);
}

test "Interpreter - execCharClassMatcher match range" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Create character class [0-9]
    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[0-9]",
        false,
        false,
        &[_]u8{},
        &[_]struct { u8, u8 }{.{ '0', '9' }},
        &[_][]const u8{},
    );
    const expr = ast.Expression{ .char_class_matcher = char_class };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("123");

    const result = try interp.execExpr(&expr);

    try testing.expectEqualStrings("1", result.string);
    try testing.expectEqual(@as(usize, 1), interp.pos);
}

test "Interpreter - execCharClassMatcher inverted" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Create inverted character class [^0-9]
    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[^0-9]",
        false,
        true, // inverted
        &[_]u8{},
        &[_]struct { u8, u8 }{.{ '0', '9' }},
        &[_][]const u8{},
    );
    const expr = ast.Expression{ .char_class_matcher = char_class };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("abc123");

    const result = try interp.execExpr(&expr);

    try testing.expectEqualStrings("a", result.string);
    try testing.expectEqual(@as(usize, 1), interp.pos);
}

test "Interpreter - execCharClassMatcher no match" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Create character class [0-9]
    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[0-9]",
        false,
        false,
        &[_]u8{},
        &[_]struct { u8, u8 }{.{ '0', '9' }},
        &[_][]const u8{},
    );
    const expr = ast.Expression{ .char_class_matcher = char_class };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("abc");

    // Should fail - 'a' not in [0-9]
    const result = interp.execExpr(&expr);
    try testing.expectError(ParseError.NoMatch, result);
}

test "Interpreter - execAnyMatcher match" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const any_matcher = try ast.AnyMatcher.create(allocator, pos, ".");
    const expr = ast.Expression{ .any_matcher = any_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("hello");

    const result = try interp.execExpr(&expr);

    try testing.expectEqualStrings("h", result.string);
    try testing.expectEqual(@as(usize, 1), interp.pos);
}

test "Interpreter - execAnyMatcher end of input" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const any_matcher = try ast.AnyMatcher.create(allocator, pos, ".");
    const expr = ast.Expression{ .any_matcher = any_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("");

    // Should fail - no input
    const result = interp.execExpr(&expr);
    try testing.expectError(ParseError.NoMatch, result);
}

test "Interpreter - position tracking with newlines" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "\n", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("\n");

    _ = try interp.execExpr(&expr);

    // After matching newline, line should increment, column should reset
    try testing.expectEqual(@as(usize, 1), interp.pos);
    try testing.expectEqual(@as(usize, 2), interp.line);
    try testing.expectEqual(@as(usize, 1), interp.column);
}

test "Interpreter - context text and position update" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("hello world");

    _ = try interp.execExpr(&expr);

    // Check Current context was updated
    try testing.expectEqualStrings("hello", interp.cur.text_buf);
    try testing.expectEqual(@as(usize, 1), interp.cur.pos.line);
    try testing.expectEqual(@as(usize, 1), interp.cur.pos.column);
    try testing.expectEqual(@as(usize, 0), interp.cur.pos.offset);
}

test "Interpreter - backtracking on failure" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    var interp = try Interpreter.init(testing.allocator, grammar);
    defer interp.deinit();

    interp.setInput("world");

    const initial_pos = interp.pos;
    const initial_line = interp.line;
    const initial_column = interp.column;

    // Should fail
    const result = interp.execExpr(&expr);
    try testing.expectError(ParseError.NoMatch, result);

    // Position should be unchanged on failure
    try testing.expectEqual(initial_pos, interp.pos);
    try testing.expectEqual(initial_line, interp.line);
    try testing.expectEqual(initial_column, interp.column);
}
