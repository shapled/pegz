const std = @import("std");
const ast = @import("../common/ast.zig");
const common_parser = @import("../common/parser.zig");
const common_lexer = @import("../common/lexer.zig");
const ebnf_lexer = @import("ebnf_lexer.zig");

// EBNF-specific parser implementation
pub const EbnfParser = struct {
    const Self = @This();

    pub fn init() Self {
        return Self{};
    }

    pub fn deinit(self: *Self) void {
        self.* = undefined;
    }

    pub fn createParser(allocator: std.mem.Allocator) !common_parser.Parser {
        const parser_ctx = try allocator.create(Self);
        parser_ctx.* = init();

        const vtable_ptr = try allocator.create(common_parser.Parser.VTable);
        vtable_ptr.* = .{
            .parseGrammar = struct {
                fn func(ctx: *anyopaque, allocator_inner: std.mem.Allocator, lxr: common_lexer.Lexer) !ast.Grammar {
                    const self = @as(*Self, @ptrCast(@alignCast(ctx)));
                    return self.parseGrammar(allocator_inner, lxr);
                }
            }.func,
            .deinit = struct {
                fn func(ctx: *anyopaque) void {
                    const self = @as(*Self, @ptrCast(@alignCast(ctx)));
                    self.deinit();
                }
            }.func,
        };

        return common_parser.Parser{
            .vtable = vtable_ptr,
            .ctx = parser_ctx,
        };
    }

    pub fn parseGrammar(self: Self, allocator: std.mem.Allocator, lxr: common_lexer.Lexer) !ast.Grammar {
        _ = self;
        var context = common_parser.ParseContext.init(allocator, lxr);
        defer context.deinit();

        var grammar = ast.Grammar.init(allocator, .ebnf);

        // Parse optional code block at the beginning
        const peeked = try context.peek();
        if (peeked.type == .code) {
            const code_token = try context.advance();
            var preamble = ast.Grammar.Preamble{};
            preamble.code = code_token.value;
            grammar.withPreamble(preamble);
        }

        // Parse rule list
        while ((try context.peek()).type != .eof) {
            const rule = try parseRule(&context);
            try grammar.addRule(rule);

            // Skip end-of-line or semicolon separators
            while (try context.peek().type == .eol or try context.peek().type == .semicolon) {
                _ = try context.advance();
            }
        }

        return grammar;
    }

    fn parseRule(context: *common_parser.ParseContext) !ast.Rule {
        // Parse rule name
        const name_token = try context.expectToken(.ident);
        const rule_name = name_token.value;

        // Parse optional display name (str | rstr | char)
        var display_name: ?[]const u8 = null;
        const peeked = try context.peek();
        if (peeked.type == .str or peeked.type == .rstr or peeked.type == .char) {
            const display_token = try context.advance();
            display_name = display_token.value;
        }

        // Parse rule definition operator
        _ = try context.expectToken(.ident); // Should be 'ruledef'

        // Parse expression
        const expression = try common_parser.ExpressionParser.parseExpression(context);

        // Parse terminator (eol | eof | semicolon)
        const terminator = try context.advance();
        if (terminator.type != .eol and terminator.type != .eof and terminator.type != .semicolon) {
            try context.errors.append(common_parser.ParseError.UnexpectedToken);
            std.log.err("Expected rule terminator, got {s} at line {d}, column {d}", .{
                terminator.type, terminator.line, terminator.column });
            return common_parser.ParseError.UnexpectedToken;
        }

        var rule = ast.Rule.init(context.allocator, rule_name, expression);
        if (display_name) |display| {
            rule = rule.withDisplayName(display);
        }

        return rule;
    }
};

// Convenience function to parse EBNF grammar from string
pub fn parseEbnfGrammar(allocator: std.mem.Allocator, input: []const u8) !ast.Grammar {
    const ebnf_lexer_impl = try ebnf_lexer.EbnfLexer.createLexer(allocator, input);
    defer ebnf_lexer_impl.deinit();

    const parser = try EbnfParser.createParser(allocator);
    defer parser.deinit();

    return parser.parseGrammar(allocator, ebnf_lexer_impl);
}