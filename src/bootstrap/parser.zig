const std = @import("std");
const ast = @import("pegz_common").ast;
const scanner_mod = @import("scan.zig");
const token_mod = @import("token.zig");
const Tid = token_mod.Tid;
const Token = token_mod.Token;
const Scanner = scanner_mod.Scanner;

const ParserError = error{
    ParseError,
    UnexpectedToken,
    MissingExpression,
    UnterminatedExpression,
    OutOfMemory,
};

/// Error list to collect multiple parse errors
const ErrList = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: std.mem.Allocator) ErrList {
        return ErrList{
            .allocator = allocator,
            .errors = .empty,
        };
    }

    pub fn reset(self: *ErrList) void {
        self.errors.clearAndFree(self.allocator);
    }

    pub fn add(self: *ErrList, pos: ast.Pos, comptime fmt: []const u8, args: anytype) void {
        const msg = std.fmt.allocPrint(self.allocator, "{f}: " ++ fmt, .{pos} ++ args) catch unreachable;
        self.errors.append(self.allocator, msg) catch unreachable;
    }

    pub fn hasErrors(self: *ErrList) bool {
        return self.errors.items.len > 0;
    }

    pub fn getError(self: *ErrList) ?[]const u8 {
        if (self.errors.items.len == 0) {
            return null;
        }
        if (self.errors.items.len == 1) {
            return self.errors.items[0];
        }

        // Combine multiple errors
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);

        for (self.errors.items, 0..) |err, i| {
            if (i > 0) {
                result.append(self.allocator, '\n') catch unreachable;
            }
            result.appendSlice(self.allocator, err) catch unreachable;
        }

        return result.toOwnedSlice(self.allocator) catch unreachable;
    }

    pub fn deinit(self: *ErrList) void {
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit(self.allocator);
    }
};

/// Parser holds the state to parse PEG grammar into an abstract syntax tree (AST).
pub const Parser = struct {
    allocator: std.mem.Allocator,
    scanner: ?Scanner,
    tok: Token,
    errs: ErrList,
    dbg: bool,
    pk: Token, // peek token

    pub fn init(allocator: std.mem.Allocator) Parser {
        return Parser{
            .allocator = allocator,
            .scanner = null,
            .tok = undefined,
            .errs = ErrList.init(allocator),
            .dbg = false,
            .pk = undefined,
        };
    }

    fn in(self: *Parser, name: []const u8) void {
        if (self.dbg) {
            std.debug.print("IN  {s} {f} {s}\n", .{ name, self.tok.id, self.tok.lit });
        }
    }

    fn out(self: *Parser, name: []const u8) void {
        if (self.dbg) {
            std.debug.print("OUT {s} {f} {s}\n", .{ name, self.tok.id, self.tok.lit });
        }
    }

    /// Parse parses the data from the reader r and generates the AST
    /// or returns an error if it fails. The filename is used as information
    /// in the error messages.
    pub fn parse(self: *Parser, filename: []const u8, reader: std.io.Reader) !*ast.Grammar {
        self.errs.reset();

        self.scanner = try Scanner.init(self.allocator, filename, reader, null);

        // advance to the first token
        self.read();

        const g = try self.grammar();

        // Clean up the scanner
        if (self.scanner) |*s| {
            s.deinit();
        }

        if (self.errs.hasErrors()) {
            if (self.errs.getError()) |err_msg| {
                self.allocator.free(err_msg);
            }
            return ParserError.ParseError;
        }

        return g;
    }

    fn read(self: *Parser) void {
        if (self.pk.pos.line != 0) {
            self.tok = self.pk;
            self.pk = std.mem.zeroes(Token);
            return;
        }

        if (self.scanner) |*s| {
            self.tok = s.scan() catch {
                self.tok = Token{
                    .id = .invalid,
                    .lit = "",
                    .pos = self.tok.pos,
                };
                return;
            };
        }
    }

    fn peek(self: *Parser) Token {
        if (self.pk.pos.line == 0) {
            if (self.scanner) |*s| {
                self.pk = s.scan() catch return Token{
                    .id = .invalid,
                    .lit = "",
                    .pos = self.tok.pos,
                };
            }
        }
        return self.pk;
    }

    fn skip(self: *Parser, ids: []const Tid) void {
        outer: while (true) {
            for (ids) |id| {
                if (self.tok.id == id) {
                    self.read();
                    continue :outer;
                }
            }
            break;
        }
    }

    fn grammar(self: *Parser) !*ast.Grammar {
        const rules: std.ArrayList(*ast.Rule) = .empty;

        defer self.out("grammar");
        self.in("grammar");

        const g = try self.allocator.create(ast.Grammar);
        g.* = ast.Grammar{
            .pos = self.tok.pos,
            .init = null,
            .rules = rules,
        };

        self.skip(&[_]Tid{ .eol, .semicolon });
        if (self.tok.id == .code) {
            const code_block = try self.allocator.create(ast.CodeBlock);
            code_block.* = ast.CodeBlock{
                .pos = self.tok.pos,
                .value = try self.allocator.dupe(u8, self.tok.lit),
            };
            g.init = code_block;
            self.read();
            self.skip(&[_]Tid{ .eol, .semicolon });
        }

        while (true) {
            if (self.tok.id == .eof) {
                return g;
            }

            const r = try self.rule();
            if (r) |rle| {
                try g.rules.append(self.allocator, rle);
            }

            self.read();
            self.skip(&[_]Tid{ .eol, .semicolon });
        }
    }

    fn expect(self: *Parser, ids: []const Tid) bool {
        if (ids.len == 0) {
            return true;
        }

        for (ids) |id| {
            if (self.tok.id == id) {
                return true;
            }
        }

        if (ids.len == 1) {
            self.errs.add(self.tok.pos, "expected {f}, got {f}", .{ ids[0], self.tok.id });
        } else {
            self.errs.add(self.tok.pos, "expected any of {any}, got {f}", .{ ids, self.tok.id });
        }
        return false;
    }

    fn rule(self: *Parser) !?*ast.Rule {
        defer self.out("rule");
        self.in("rule");

        if (!self.expect(&[_]Tid{.ident})) {
            return null;
        }

        const rle = try self.allocator.create(ast.Rule);
        rle.* = ast.Rule{
            .pos = self.tok.pos,
            .name = undefined,
            .display_name = undefined,
            .expression = undefined,
            .expr = undefined,
            .visited = false,
            .nullable = false,
            .left_recursive = false,
            .leader = false,
        };

        const identifier = try self.allocator.create(ast.Identifier);
        identifier.* = ast.Identifier{
            .pos = self.tok.pos,
            .value = try self.allocator.dupe(u8, self.tok.lit),
        };
        rle.name = identifier.*;
        self.read();

        if (self.tok.id == .str or self.tok.id == .rstr or self.tok.id == .char) {
            if (std.mem.endsWith(u8, self.tok.lit, "i")) {
                self.errs.add(self.tok.pos, "invalid suffix 'i'", .{});
                return null;
            }

            // Unquote the string (simplified version - would need proper implementation)
            const unquoted = try self.unquote(self.tok.lit);
            const display_name = try self.allocator.create(ast.StringLit);
            display_name.* = ast.StringLit{
                .pos = self.tok.pos,
                .value = unquoted,
            };
            rle.display_name = display_name.*;
            self.read();
        }

        if (!self.expect(&[_]Tid{.ruledef})) {
            return null;
        }
        self.read();
        self.skip(&[_]Tid{.eol});

        const expr = try self.expression();
        if (expr == null) {
            self.errs.add(self.tok.pos, "missing expression", .{});
            return null;
        }
        const expr_ptr = try self.allocator.create(ast.Expression);
        expr_ptr.* = expr.?;
        rle.expr = expr_ptr;
        rle.expression = expr.?;

        if (!self.expect(&[_]Tid{ .eol, .eof, .semicolon })) {
            self.errs.add(self.tok.pos, "rule not terminated", .{});
            return null;
        }
        return rle;
    }

    fn expression(self: *Parser) ParserError!?ast.Expression {
        defer self.out("expression");
        self.in("expression");

        var alternatives = try std.ArrayList(*ast.Expression).initCapacity(self.allocator, 0);
        defer alternatives.deinit(self.allocator);

        while (true) {
            const expr = try self.actionExpr();
            if (expr) |e| {
                const e_ptr = try self.allocator.create(ast.Expression);
                e_ptr.* = e;
                try alternatives.append(self.allocator, e_ptr);
            }

            if (self.tok.id != .slash) {
                switch (alternatives.items.len) {
                    0 => {
                        self.errs.add(self.tok.pos, "no expression in choice", .{});
                        return null;
                    },
                    1 => {
                        return alternatives.items[0].*;
                    },
                    else => {
                        const choice = try self.allocator.create(ast.ChoiceExpr);
                        choice.* = ast.ChoiceExpr{
                            .pos = alternatives.items[0].pos(),
                            .alternatives = try std.ArrayList(*ast.Expression).initCapacity(self.allocator, 0),
                            .nullable = false,
                        };

                        for (alternatives.items) |alt| {
                            try choice.alternatives.append(self.allocator, alt);
                        }

                        return ast.Expression{ .choice = choice };
                    },
                }
            }
            // move after the slash
            self.read();
        }
    }

    fn actionExpr(self: *Parser) !?ast.Expression {
        defer self.out("actionExpr");
        self.in("actionExpr");

        const expr = try self.seqExpr();
        if (expr == null) {
            return null;
        }

        var code_block: ?*ast.CodeBlock = null;
        if (self.tok.id == .code) {
            code_block = try self.allocator.create(ast.CodeBlock);
            code_block.?.* = ast.CodeBlock{
                .pos = self.tok.pos,
                .value = try self.allocator.dupe(u8, self.tok.lit),
            };
            self.read();
        }

        if (code_block) |code| {
            const action = try self.allocator.create(ast.ActionExpr);
            const expr_ptr = try self.allocator.create(ast.Expression);
            expr_ptr.* = expr.?;
            action.* = ast.ActionExpr{
                .pos = expr.?.pos(),
                .expr = expr_ptr,
                .code = code,
                .func_ix = 0,
                .nullable = false,
            };
            return ast.Expression{ .action = action };
        }

        return expr;
    }

    fn seqExpr(self: *Parser) !?ast.Expression {
        defer self.out("seqExpr");
        self.in("seqExpr");

        var exprs = try std.ArrayList(*ast.Expression).initCapacity(self.allocator, 0);
        defer exprs.deinit(self.allocator);

        while (true) {
            const expr = try self.labeledExpr();
            if (expr) |e| {
                const e_ptr = try self.allocator.create(ast.Expression);
                e_ptr.* = e;
                try exprs.append(self.allocator, e_ptr);
            } else {
                switch (exprs.items.len) {
                    0 => {
                        self.errs.add(self.tok.pos, "no expression in sequence", .{});
                        return null;
                    },
                    1 => {
                        return exprs.items[0].*;
                    },
                    else => {
                        const seq = try self.allocator.create(ast.SeqExpr);
                        seq.* = ast.SeqExpr{
                            .pos = exprs.items[0].pos(),
                            .exprs = try std.ArrayList(*ast.Expression).initCapacity(self.allocator, 0),
                            .nullable = false,
                        };

                        for (exprs.items) |e| {
                            try seq.exprs.append(self.allocator, e);
                        }

                        return ast.Expression{ .seq = seq };
                    },
                }
            }
        }
    }

    fn labeledExpr(self: *Parser) !?ast.Expression {
        defer self.out("labeledExpr");
        self.in("labeledExpr");

        if (self.tok.id != .ident) {
            return null;
        }

        const peek_token = self.peek();
        if (peek_token.id == .colon) {
            return null;
        }

        const identifier: *ast.Identifier = try self.allocator.create(ast.Identifier);
        identifier.* = ast.Identifier{
            .pos = self.tok.pos,
            .value = try self.allocator.dupe(u8, self.tok.lit),
        };
        self.read();
        if (!self.expect(&[_]Tid{.colon})) {
            return null;
        }
        self.read();

        const expr = try self.prefixedExpr();
        if (expr == null) {
            self.errs.add(self.tok.pos, "label without expression", .{});
            return null;
        }

        const labeled = try self.allocator.create(ast.LabeledExpr);
        const expr_ptr = try self.allocator.create(ast.Expression);
        expr_ptr.* = expr.?;
        labeled.* = ast.LabeledExpr{
            .pos = identifier.pos,
            .label = identifier,
            .expr = expr_ptr,
        };
        return ast.Expression{ .labeled = labeled };
    }

    fn prefixedExpr(self: *Parser) !?ast.Expression {
        defer self.out("prefixedExpr");
        self.in("prefixedExpr");

        var prefix_type: ?Tid = null;
        if (self.tok.id == .ampersand or self.tok.id == .exclamation) {
            prefix_type = self.tok.id;
            self.read();
        }

        const expr = try self.suffixedExpr();
        if (expr == null) {
            if (prefix_type != null) {
                self.errs.add(self.tok.pos, "prefix operator without expression", .{});
            }
            return null;
        }

        if (prefix_type) |pt| {
            switch (pt) {
                .ampersand => {
                    const and_expr = try self.allocator.create(ast.AndExpr);
                    const expr_ptr = try self.allocator.create(ast.Expression);
                    expr_ptr.* = expr.?;
                    and_expr.* = ast.AndExpr{
                        .pos = expr.?.pos(),
                        .expr = expr_ptr,
                    };
                    return ast.Expression{ .and_expr = and_expr };
                },
                .exclamation => {
                    const not_expr = try self.allocator.create(ast.NotExpr);
                    const expr_ptr = try self.allocator.create(ast.Expression);
                    expr_ptr.* = expr.?;
                    not_expr.* = ast.NotExpr{
                        .pos = expr.?.pos(),
                        .expr = expr_ptr,
                    };
                    return ast.Expression{ .not = not_expr };
                },
                else => return expr,
            }
        }

        return expr;
    }

    fn suffixedExpr(self: *Parser) !?ast.Expression {
        defer self.out("suffixedExpr");
        self.in("suffixedExpr");

        const expr = try self.primaryExpr();
        if (expr == null) {
            if (self.tok.id == .question or self.tok.id == .star or self.tok.id == .plus) {
                self.errs.add(self.tok.pos, "suffix operator without expression", .{});
            }
            return null;
        }

        switch (self.tok.id) {
            .question => {
                const zero_or_one = try self.allocator.create(ast.ZeroOrOneExpr);
                const expr_ptr = try self.allocator.create(ast.Expression);
                expr_ptr.* = expr.?;
                zero_or_one.* = ast.ZeroOrOneExpr{
                    .pos = expr.?.pos(),
                    .expr = expr_ptr,
                };
                self.read();
                return ast.Expression{ .zero_or_one = zero_or_one };
            },
            .star => {
                const zero_or_more = try self.allocator.create(ast.ZeroOrMoreExpr);
                const expr_ptr = try self.allocator.create(ast.Expression);
                expr_ptr.* = expr.?;
                zero_or_more.* = ast.ZeroOrMoreExpr{
                    .pos = expr.?.pos(),
                    .expr = expr_ptr,
                };
                self.read();
                return ast.Expression{ .zero_or_more = zero_or_more };
            },
            .plus => {
                const one_or_more = try self.allocator.create(ast.OneOrMoreExpr);
                const expr_ptr = try self.allocator.create(ast.Expression);
                expr_ptr.* = expr.?;
                one_or_more.* = ast.OneOrMoreExpr{
                    .pos = expr.?.pos(),
                    .expr = expr_ptr,
                };
                self.read();
                return ast.Expression{ .one_or_more = one_or_more };
            },
            else => return expr,
        }
    }

    fn primaryExpr(self: *Parser) !?ast.Expression {
        defer self.out("primaryExpr");
        self.in("primaryExpr");

        switch (self.tok.id) {
            .str, .rstr, .char => {
                // literal matcher
                var ignore_case = false;
                var lit_str = self.tok.lit;

                if (std.mem.endsWith(u8, lit_str, "i")) {
                    ignore_case = true;
                    lit_str = lit_str[0 .. lit_str.len - 1];
                }

                const unquoted = try self.unquote(lit_str);
                const lit_matcher = try self.allocator.create(ast.LitMatcher);
                lit_matcher.* = ast.LitMatcher{
                    .pos = self.tok.pos,
                    .value = unquoted,
                    .ignore_case = ignore_case,
                };
                self.read();
                return ast.Expression{ .lit_matcher = lit_matcher };
            },

            .class => {
                // character class matcher
                const char_class = try self.allocator.create(ast.CharClassMatcher);
                const charts: std.ArrayList(u8) = .empty;
                char_class.* = ast.CharClassMatcher{
                    .pos = self.tok.pos,
                    .value = try self.allocator.dupe(u8, self.tok.lit),
                    .ignore_case = false,
                    .inverted = false,
                    .chars = charts,
                    .ranges = try std.ArrayList(struct { u8, u8 }).initCapacity(self.allocator, 0),
                    .unicode_classes = try std.ArrayList([]const u8).initCapacity(self.allocator, 0),
                };
                self.read();
                return ast.Expression{ .char_class_matcher = char_class };
            },

            .dot => {
                // any matcher
                const any_matcher = try self.allocator.create(ast.AnyMatcher);
                any_matcher.* = ast.AnyMatcher{
                    .pos = self.tok.pos,
                    .value = try self.allocator.dupe(u8, self.tok.lit),
                };
                self.read();
                return ast.Expression{ .any_matcher = any_matcher };
            },

            .ident => {
                // rule reference expression
                return self.ruleRefExpr();
            },

            .lparen => {
                // expression in parenthesis
                self.read();
                const expr = try self.expression();
                if (expr == null) {
                    self.errs.add(self.tok.pos, "missing expression inside parenthesis", .{});
                    return null;
                }
                if (!self.expect(&[_]Tid{.rparen})) {
                    return null;
                }
                self.read();
                return expr;
            },

            else => {
                return null;
            },
        }
    }

    fn ruleRefExpr(self: *Parser) !?ast.Expression {
        defer self.out("ruleRefExpr");
        self.in("ruleRefExpr");

        if (!self.expect(&[_]Tid{.ident})) {
            return null;
        }

        const identifier = try self.allocator.create(ast.Identifier);
        identifier.* = ast.Identifier{
            .pos = self.tok.pos,
            .value = try self.allocator.dupe(u8, self.tok.lit),
        };

        const rule_ref = try self.allocator.create(ast.RuleRefExpr);
        rule_ref.* = ast.RuleRefExpr{
            .pos = self.tok.pos,
            .name = identifier,
            .nullable = false,
        };

        self.read();
        return ast.Expression{ .rule_ref = rule_ref };
    }

    /// Simple unquote implementation - would need to be more complete
    fn unquote(self: *Parser, quoted: []const u8) ![]const u8 {
        if (quoted.len < 2) {
            return quoted;
        }

        // Remove surrounding quotes and handle basic escapes
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(self.allocator);

        const content = quoted[1 .. quoted.len - 1];
        var i: usize = 0;

        while (i < content.len) {
            if (content[i] == '\\') {
                if (i + 1 < content.len) {
                    switch (content[i + 1]) {
                        'n' => try result.append(self.allocator, '\n'),
                        't' => try result.append(self.allocator, '\t'),
                        'r' => try result.append(self.allocator, '\r'),
                        '\\' => try result.append(self.allocator, '\\'),
                        '"' => try result.append(self.allocator, '"'),
                        '\'' => try result.append(self.allocator, '\''),
                        else => {
                            try result.append(self.allocator, content[i]);
                            try result.append(self.allocator, content[i + 1]);
                        },
                    }
                    i += 2;
                } else {
                    try result.append(self.allocator, content[i]);
                    i += 1;
                }
            } else {
                try result.append(self.allocator, content[i]);
                i += 1;
            }
        }

        return result.toOwnedSlice(self.allocator);
    }

    pub fn deinit(self: *Parser) void {
        self.errs.deinit();
    }
};
