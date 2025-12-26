//! Unit tests for Token types
//! Task 4.3: Tokenizer 测试

const std = @import("std");
const bootstrap = @import("bootstrap");
const Token = bootstrap.token.Token;
const Tid = bootstrap.token.Tid;
const ast = @import("pegz_common").ast;

const testing = std.testing;

test "Token - create identifier token" {
    const token = Token{
        .id = Tid.ident,
        .lit = "Identifier",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 1,
            .column = 1,
            .offset = 0,
        },
    };

    try testing.expectEqual(Tid.ident, token.id);
    try testing.expectEqualStrings("Identifier", token.lit);
    try testing.expectEqual(@as(usize, 1), token.pos.line);
    try testing.expectEqual(@as(usize, 1), token.pos.column);
}

test "Token - create string literal token" {
    const token = Token{
        .id = Tid.str,
        .lit = "\"hello\"",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 5,
            .column = 10,
            .offset = 42,
        },
    };

    try testing.expectEqual(Tid.str, token.id);
    try testing.expectEqualStrings("\"hello\"", token.lit);
    try testing.expectEqual(@as(usize, 5), token.pos.line);
}

test "Token - create char literal token" {
    const token = Token{
        .id = Tid.char,
        .lit = "'a'",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 1,
            .column = 1,
            .offset = 0,
        },
    };

    try testing.expectEqual(Tid.char, token.id);
    try testing.expectEqualStrings("'a'", token.lit);
}

test "Token - create character class token" {
    const token = Token{
        .id = Tid.class,
        .lit = "[a-zA-Z]",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 3,
            .column = 5,
            .offset = 20,
        },
    };

    try testing.expectEqual(Tid.class, token.id);
    try testing.expectEqualStrings("[a-zA-Z]", token.lit);
}

test "Token - create operator tokens" {
    const operators = [_]struct {
        id: Tid,
        lit: []const u8,
    }{
        .{ .id = Tid.dot, .lit = "." },
        .{ .id = Tid.ampersand, .lit = "&" },
        .{ .id = Tid.exclamation, .lit = "!" },
        .{ .id = Tid.question, .lit = "?" },
        .{ .id = Tid.plus, .lit = "+" },
        .{ .id = Tid.star, .lit = "*" },
        .{ .id = Tid.slash, .lit = "/" },
        .{ .id = Tid.colon, .lit = ":" },
        .{ .id = Tid.semicolon, .lit = ";" },
        .{ .id = Tid.lparen, .lit = "(" },
        .{ .id = Tid.rparen, .lit = ")" },
    };

    for (operators) |op| {
        const token = Token{
            .id = op.id,
            .lit = op.lit,
            .pos = ast.Pos{
                .filename = "test.pegz",
                .line = 1,
                .column = 1,
                .offset = 0,
            },
        };

        try testing.expectEqual(op.id, token.id);
        try testing.expectEqualStrings(op.lit, token.lit);
    }
}

test "Token - create rule definition token" {
    const token = Token{
        .id = Tid.ruledef,
        .lit = "<-",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 1,
            .column = 5,
            .offset = 4,
        },
    };

    try testing.expectEqual(Tid.ruledef, token.id);
    try testing.expectEqualStrings("<-", token.lit);
}

test "Token - EOF token" {
    const token = Token{
        .id = Tid.eof,
        .lit = "",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 10,
            .column = 1,
            .offset = 50,
        },
    };

    try testing.expectEqual(Tid.eof, token.id);
    try testing.expectEqual(@as(usize, 0), token.lit.len);
}

test "Token - position tracking" {
    const pos1 = ast.Pos{
        .filename = "test.pegz",
        .line = 1,
        .column = 1,
        .offset = 0,
    };

    const pos2 = ast.Pos{
        .filename = "test.pegz",
        .line = 2,
        .column = 5,
        .offset = 10,
    };

    // Test line increment
    try testing.expectEqual(@as(usize, 1), pos1.line);
    try testing.expectEqual(@as(usize, 2), pos2.line);

    // Test column position
    try testing.expectEqual(@as(usize, 1), pos1.column);
    try testing.expectEqual(@as(usize, 5), pos2.column);

    // Test offset
    try testing.expectEqual(@as(usize, 0), pos1.offset);
    try testing.expectEqual(@as(usize, 10), pos2.offset);
}

test "Token - invalid token" {
    const token = Token{
        .id = Tid.invalid,
        .lit = "",
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 0,
            .column = 0,
            .offset = 0,
        },
    };

    try testing.expectEqual(Tid.invalid, token.id);
}

test "Token - Tid enum values" {
    // Test that enum values match their ASCII counterparts
    try testing.expectEqual(@as(i32, '\n'), @intFromEnum(Tid.eol));
    try testing.expectEqual(@as(i32, ':'), @intFromEnum(Tid.colon));
    try testing.expectEqual(@as(i32, ';'), @intFromEnum(Tid.semicolon));
    try testing.expectEqual(@as(i32, '('), @intFromEnum(Tid.lparen));
    try testing.expectEqual(@as(i32, ')'), @intFromEnum(Tid.rparen));
    try testing.expectEqual(@as(i32, '.'), @intFromEnum(Tid.dot));
    try testing.expectEqual(@as(i32, '&'), @intFromEnum(Tid.ampersand));
    try testing.expectEqual(@as(i32, '!'), @intFromEnum(Tid.exclamation));
    try testing.expectEqual(@as(i32, '?'), @intFromEnum(Tid.question));
    try testing.expectEqual(@as(i32, '+'), @intFromEnum(Tid.plus));
    try testing.expectEqual(@as(i32, '*'), @intFromEnum(Tid.star));
    try testing.expectEqual(@as(i32, '/'), @intFromEnum(Tid.slash));
}

test "Token - long literal formatting" {
    // Test that tokens with long literals are handled correctly
    const long_lit = "very_long_identifier_name_that_exceeds_normal_length";

    const token = Token{
        .id = Tid.ident,
        .lit = long_lit,
        .pos = ast.Pos{
            .filename = "test.pegz",
            .line = 1,
            .column = 1,
            .offset = 0,
        },
    };

    try testing.expectEqual(Tid.ident, token.id);
    try testing.expectEqualStrings(long_lit, token.lit);
    try testing.expectEqual(long_lit.len, token.lit.len);
}
