const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");

// Common parser interface for both EBNF and PEGZ parsers
pub const Parser = struct {
    const Self = @This();

    // VTable for different parser implementations
    pub const VTable = struct {
        parseGrammar: *const fn (ctx: *anyopaque, allocator: std.mem.Allocator, lxr: lexer.Lexer) anyerror!ast.Grammar,
        deinit: *const fn (ctx: *anyopaque) void,
    };

    vtable: *const VTable,
    ctx: *anyopaque,

    pub fn parseGrammar(self: Self, allocator: std.mem.Allocator, lxr: lexer.Lexer) !ast.Grammar {
        return self.vtable.parseGrammar(self.ctx, allocator, lxr);
    }

    pub fn deinit(self: Self) void {
        self.vtable.deinit(self.ctx);
    }
};

// Parse error types
pub const ParseError = error{
    UnexpectedToken,
    UnexpectedEOF,
    UnterminatedString,
    UnterminatedChar,
    UnterminatedCharClass,
    UnterminatedCodeBlock,
    DuplicateRule,
    InvalidExpression,
    MemoryAllocation,
    InvalidGrammar,
} || std.mem.Allocator.Error;

// Common parsing utilities
pub const ParseContext = struct {
    allocator: std.mem.Allocator,
    lxr: lexer.Lexer,
    current_token: ?lexer.Token = null,
    errors: std.ArrayList(ParseError),

    pub fn init(allocator: std.mem.Allocator, lxr: lexer.Lexer) ParseContext {
        return ParseContext{
            .allocator = allocator,
            .lxr = lxr,
            .errors = std.ArrayList(ParseError).initCapacity(allocator, 0) catch unreachable,
        };
    }

    pub fn deinit(self: *ParseContext) void {
        self.errors.deinit(self.allocator);
    }

    pub fn advance(self: *ParseContext) !lexer.Token {
        if (self.current_token) |token| {
            self.current_token = null;
            return token;
        }
        return self.lxr.nextToken();
    }

    pub fn peek(self: *ParseContext) !lexer.Token {
        if (self.current_token) |token| {
            return token;
        }
        self.current_token = try self.lxr.nextToken();
        return self.current_token.?;
    }

    pub fn expectToken(self: *ParseContext, expected_type: lexer.TokenType) !lexer.Token {
        const token = try self.advance();
        if (token.type != expected_type) {
            try self.errors.append(self.allocator, ParseError.UnexpectedToken);
            std.log.err("Expected token {s}, got {s} at line {d}, column {d}", .{
                expected_type, token.type, token.line, token.column });
            return ParseError.UnexpectedToken;
        }
        return token;
    }

    pub fn expectIdentifier(self: *ParseContext, expected_name: []const u8) !lexer.Token {
        const token = try self.expectToken(.ident);
        if (!std.mem.eql(u8, token.value, expected_name)) {
            try self.errors.append(self.allocator, ParseError.UnexpectedToken);
            std.log.err("Expected identifier '{s}', got '{s}' at line {d}, column {d}", .{
                expected_name, token.value, token.line, token.column });
            return ParseError.UnexpectedToken;
        }
        return token;
    }

    pub fn consumeOptionalToken(self: *ParseContext, token_type: lexer.TokenType) ?lexer.Token {
        const peeked = self.peek() catch return null;
        if (peeked.type == token_type) {
            return self.advance() catch null;
        }
        return null;
    }

    pub fn isAtEnd(self: *ParseContext) !bool {
        const peeked = try self.peek();
        return peeked.type == .eof;
    }

    pub fn synchronize(self: *ParseContext) !void {
        // Skip tokens until we find a rule boundary
        while (try self.peek().type != .eof) {
            const token = try self.advance();
            if (token.type == .semicolon or token.type == .eol) {
                return;
            }
        }
    }
};

// Expression parsing utilities
pub const ExpressionParser = struct {
    // Parse an expression with the lowest precedence (choice)
    pub fn parseExpression(context: *ParseContext) !ast.Expression {
        return parseChoiceExpression(context);
    }

    // Parse choice expression (lowest precedence)
    fn parseChoiceExpression(context: *ParseContext) !ast.Expression {
        var left = try parseActionExpression(context);

        while (try context.peek().type == .slash) {
            _ = try context.advance(); // Consume '/'
            const right = try parseActionExpression(context);

            // Create a new choice expression
            const choice = try context.allocator.create(ast.ChoiceExpr);
            choice.* = ast.ChoiceExpr.init(context.allocator);
            try choice.addAlternative(left.action);
            try choice.addAlternative(right);

            left = ast.ActionExpr.init(ast.SeqExpr.init(context.allocator));
            left.action_code = null;
        }

        return ast.Expression{ .action = left };
    }

    // Parse action expression
    fn parseActionExpression(context: *ParseContext) !ast.ActionExpr {
        const sequence = try parseSequenceExpression(context);
        var action = ast.ActionExpr.init(sequence);

        // Check for action code { ... }
        if (try context.peek().type == .code) {
            const code_token = try context.advance();
            action.action_code = code_token.value;
        }

        return action;
    }

    // Parse sequence expression
    fn parseSequenceExpression(context: *ParseContext) !ast.SeqExpr {
        const sequence = try context.allocator.create(ast.SeqExpr);
        sequence.* = ast.SeqExpr.init(context.allocator);

        while (true) {
            const peeked = try context.peek();
            if (peeked.type == .eof or peeked.type == .slash or
                peeked.type == .rparen or peeked.type == .semicolon or
                peeked.type == .eol) {
                break;
            }
            const expr = try parseLabeledExpression(context);
            try sequence.addExpression(expr);

            // Break if we find a code block (belongs to action expression, not sequence)
            const code_peek = try context.peek();
            if (code_peek.type == .code) {
                break;
            }
        }

        return sequence.*;
    }

    // Parse labeled expression
    fn parseLabeledExpression(context: *ParseContext) !ast.LabeledExpr {
        const peeked = try context.peek();

        // Check for label:expression pattern
        if (peeked.type == .ident) {
            const next_peek = try context.lxr.peekToken();
            if (next_peek.type == .colon) {
                const label_token = try context.advance(); // identifier
                _ = try context.advance(); // colon

                const prefixed = try parsePrefixedExpression(context);
                return prefixed.withLabel(label_token.value);
            }
        }

        const prefixed = try parsePrefixedExpression(context);
        return ast.LabeledExpr.init(prefixed);
    }

    // Parse prefixed expression
    fn parsePrefixedExpression(context: *ParseContext) !ast.PrefixedExpr {
        const peeked = try context.peek();

        if (peeked.type == .ampersand) {
            _ = try context.advance(); // consume '&'
            const expr = try parsePrefixedExpression(context);
            return expr.withOp(.and_op);
        } else if (peeked.type == .exclamation) {
            _ = try context.advance(); // consume '!'
            const expr = try parsePrefixedExpression(context);
            return expr.withOp(.not_op);
        }

        const suffixed = try parseSuffixedExpression(context);
        return ast.PrefixedExpr.init(suffixed);
    }

    // Parse suffixed expression
    fn parseSuffixedExpression(context: *ParseContext) !ast.SuffixedExpr {
        const primary = try parsePrimaryExpression(context);
        var suffixed = ast.SuffixedExpr.init(primary);

        const peeked = try context.peek();
        switch (peeked.type) {
            .question => {
                _ = try context.advance();
                suffixed.op = .optional;
            },
            .star => {
                _ = try context.advance();
                suffixed.op = .zero_or_more;
            },
            .plus => {
                _ = try context.advance();
                suffixed.op = .one_or_more;
            },
            else => {},
        }

        return suffixed;
    }

    // Parse primary expression
    fn parsePrimaryExpression(context: *ParseContext) !ast.PrimaryExpr {
        const token = try context.advance();

        switch (token.type) {
            .str, .rstr, .char => {
                const literal_type: ast.LiteralMatcher.LiteralType = switch (token.type) {
                    .str => .string,
                    .rstr => .raw_string,
                    .char => .character,
                    else => unreachable,
                };
                const literal = ast.LiteralMatcher.init(token.value, literal_type);
                return ast.PrimaryExpr{ .literal = literal };
            },

            .class => {
                const char_class = ast.CharClassMatcher.init(token.value);
                return ast.PrimaryExpr{ .char_class = char_class };
            },

            .dot => {
                const any_matcher = ast.AnyMatcher.init();
                return ast.PrimaryExpr{ .any_matcher = any_matcher };
            },

            .ident => {
                const rule_ref = ast.RuleRefExpr.init(token.value);
                return ast.PrimaryExpr{ .rule_ref = rule_ref };
            },

            .lparen => {
                const expr = parseExpression(context) catch |err| return err;
                _ = try context.expectToken(.rparen);
                const expr_ptr = try context.allocator.create(ast.Expression);
                expr_ptr.* = expr;
                const grouped = ast.GroupedExpr.init(expr_ptr);
                return ast.PrimaryExpr{ .grouped = grouped };
            },

            else => {
                try context.errors.append(ParseError.UnexpectedToken);
                std.log.err("Unexpected token {s} in primary expression at line {d}, column {d}", .{
                    token.type, token.line, token.column });
                return ParseError.UnexpectedToken;
            },
        }
    }
};