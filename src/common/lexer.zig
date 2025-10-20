const std = @import("std");

// Common token types and lexer interface for both EBNF and PEGZ

pub const TokenType = enum {
    // Identifiers and literals
    ident,
    str,
    rstr,
    char,
    code,

    // Operators and punctuation
    colon,       // :
    dot,         // .
    semicolon,   // ;
    lparen,      // (
    rparen,      // )
    lbrace,      // {
    rbrace,      // }
    lbracket,    // [
    rbracket,    // ]

    // Special operators
    ampersand,   // &
    exclamation, // !
    question,    // ?
    star,        // *
    plus,        // +
    slash,       // /
    equals,      // =
    pipe,        // |

    // Special tokens
    class,       // character class [abc]

    // Control tokens
    eol,
    eof,
    invalid,

    };

pub const Token = struct {
    type: TokenType,
    value: []const u8,
    line: usize,
    column: usize,

    };

// Lexer interface that both EBNF and PEGZ lexers will implement
pub const Lexer = struct {
    const Self = @This();

    // VTable for different lexer implementations
    pub const VTable = struct {
        nextToken: *const fn (ctx: *anyopaque) anyerror!Token,
        peekToken: *const fn (ctx: *anyopaque) anyerror!Token,
        deinit: *const fn (ctx: *anyopaque) void,
    };

    vtable: *const VTable,
    ctx: *anyopaque,

    pub fn nextToken(self: Self) !Token {
        return self.vtable.nextToken(self.ctx);
    }

    pub fn peekToken(self: Self) !Token {
        return self.vtable.peekToken(self.ctx);
    }

    pub fn deinit(self: Self) void {
        self.vtable.deinit(self.ctx);
    }
};

// Utility functions that work with any lexer implementation
pub fn expectToken(lexer: Lexer, expected_type: TokenType) !Token {
    const token = try lexer.nextToken();
    if (token.type != expected_type) {
        std.log.err("Expected token {s}, got {s} at line {d}, column {d}", .{
            expected_type, token.type, token.line, token.column });
        return error.UnexpectedToken;
    }
    return token;
}

pub fn expectIdentifier(lexer: Lexer, expected_name: []const u8) !Token {
    const token = try expectToken(lexer, .ident);
    if (!std.mem.eql(u8, token.value, expected_name)) {
        std.log.err("Expected identifier '{s}', got '{s}' at line {d}, column {d}", .{
            expected_name, token.value, token.line, token.column });
        return error.UnexpectedIdentifier;
    }
    return token;
}

pub fn skipUntilToken(lexer: Lexer, token_types: []const TokenType) !Token {
    while (true) {
        const token = try lexer.nextToken();
        for (token_types) |target_type| {
            if (token.type == target_type) {
                return token;
            }
        }
        if (token.type == .eof) {
            return token;
        }
    }
}

pub fn isExpressionToken(token_type: TokenType) bool {
    return switch (token_type) {
        .ident, .str, .rstr, .char, .class, .dot, .lparen, .ampersand, .exclamation => true,
        else => false,
    };
}

pub fn isPrefixOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .ampersand, .exclamation => true,
        else => false,
    };
}

pub fn isSuffixOperator(token_type: TokenType) bool {
    return switch (token_type) {
        .question, .star, .plus => true,
        else => false,
    };
}

pub fn isLiteralToken(token_type: TokenType) bool {
    return switch (token_type) {
        .str, .rstr, .char => true,
        else => false,
    };
}