//! Unit tests for AST nodes (minimal)
//! Task 4.3: AST 测试

const std = @import("std");
const ast = @import("pegz_common").ast;

const testing = std.testing;

test "Pos - create position" {
    const pos = ast.Pos{
        .filename = "test.pegz",
        .line = 5,
        .column = 10,
        .offset = 42,
    };

    try testing.expectEqual(@as(usize, 5), pos.line);
    try testing.expectEqual(@as(usize, 10), pos.column);
    try testing.expectEqual(@as(usize, 42), pos.offset);
}

test "Pos - init helper" {
    const pos = ast.Pos.init(1, 2, 3);

    try testing.expectEqual(@as(usize, 1), pos.line);
    try testing.expectEqual(@as(usize, 2), pos.column);
    try testing.expectEqual(@as(usize, 3), pos.offset);
}

test "Identifier - create identifier" {
    const pos = ast.Pos.init(1, 1, 0);
    const ident = ast.Identifier{
        .pos = pos,
        .value = "myRule",
    };

    try testing.expectEqualStrings("myRule", ident.value);
}

test "StringLit - create string literal" {
    const pos = ast.Pos.init(5, 10, 42);
    const str_lit = ast.StringLit{
        .pos = pos,
        .value = "hello",
    };

    try testing.expectEqualStrings("hello", str_lit.value);
}

test "Grammar - create empty grammar" {
    const pos = ast.Pos.init(1, 1, 0);
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const grammar = try ast.Grammar.create(arena.allocator(), pos, null);

    try testing.expectEqual(@as(usize, 0), grammar.rules.items.len);
}

test "LitMatcher - create literal matcher" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);

    try testing.expectEqualStrings("hello", lit_matcher.value);
}

test "CharClassMatcher - create character class" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[a-z]",
        false,
        false,
        &[_]u8{},
        &[_]struct { u8, u8 }{},
        &[_][]const u8{},
    );

    try testing.expectEqual(false, char_class.inverted);
}

test "AnyMatcher - create any matcher" {
    const pos = ast.Pos.init(1, 1, 0);
    const any_matcher = ast.AnyMatcher{
        .pos = pos,
        .value = ".",
    };

    try testing.expectEqualStrings(".", any_matcher.value);
}
