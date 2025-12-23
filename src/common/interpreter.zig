const std = @import("std");
const ast = @import("ast.zig");

/// Runtime value type used by the interpreter
/// This represents the result of parsing expressions
pub const Value = union(enum) {
    null: void,
    boolean: bool,
    integer: i64,
    string: []const u8,
    identifier: *const ast.Identifier,
    expression: *const ast.Expression,
    code_block: *const ast.CodeBlock,
    rule: *const ast.Rule,
    grammar: *const ast.Grammar,
    list: std.ArrayList(Value),

    pub fn format(self: *const Value, writer: anytype) !void {
        switch (self.*) {
            .null => try writer.writeAll("null"),
            .boolean => |b| try writer.print("{}", .{b}),
            .integer => |i| try writer.print("{}", .{i}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .identifier => |id| try writer.print("Identifier({s})", .{id.value}),
            else => try writer.writeAll("..."),
        }
    }
};

/// Current context object, provides helper methods to action code
/// Similar to Pigeon's "current" struct
pub const Current = struct {
    allocator: std.mem.Allocator,
    vstack: *std.ArrayList(Value),
    text_buf: []const u8,
    pos: ast.Pos,

    pub fn astPos(self: *const Current) ast.Pos {
        return self.pos;
    }

    pub fn text(self: *const Current) []const u8 {
        return self.text_buf;
    }

    pub fn create(self: *Current, comptime T: type) !*T {
        return self.allocator.create(T);
    }

    pub fn dup(self: *Current, comptime T: type, value: T) !T {
        switch (@typeInfo(T)) {
            .Pointer => |ptr| {
                switch (ptr.child) {
                    u8 => return @as(T, @ptrCast(self.allocator.dupe(u8, value))),
                    else => @compileError("dup not implemented for this pointer type"),
                }
            },
            else => return value,
        }
    }
};

/// Parsing errors
pub const ParseError = error{
    NoMatch,
    EndOfStream,
    RuleNotFound,
    UnimplementedExpression,
    UnimplementedMatcher,
};

/// Interpreter engine - executes expression trees at runtime
pub const Interpreter = struct {
    allocator: std.mem.Allocator,
    grammar: *const ast.Grammar,
    vstack: std.ArrayList(Value),
    cur: *Current,
    rule_funcs: std.StringHashMap(*const fn (*Current, []const Value) anyerror!Value),

    // Parser state
    input: []const u8 = "",
    pos: usize = 0,  // Current position in input
    line: usize = 1,
    column: usize = 1,

    pub fn init(allocator: std.mem.Allocator, grammar: *const ast.Grammar) !Interpreter {
        var interp = Interpreter{
            .allocator = allocator,
            .grammar = grammar,
            .vstack = std.ArrayList(Value).init(allocator),
            .cur = undefined,
            .rule_funcs = std.StringHashMap(*const fn (*Current, []const Value) anyerror!Value).init(allocator),
        };

        const current_obj = try allocator.create(Current);
        current_obj.* = Current{
            .allocator = allocator,
            .vstack = &interp.vstack,
            .text_buf = "",
            .pos = ast.Pos{ .line = 0, .column = 0, .offset = 0 },
        };
        interp.cur = current_obj;

        return interp;
    }

    pub fn deinit(self: *Interpreter) void {
        self.vstack.deinit(self.allocator);
        self.rule_funcs.deinit(self.allocator);
        self.allocator.destroy(self.cur);
    }

    /// Set input text for parsing
    pub fn setInput(self: *Interpreter, input: []const u8) void {
        self.input = input;
        self.pos = 0;
        self.line = 1;
        self.column = 1;
    }

    /// Get current character in input
    fn currentChar(self: *const Interpreter) ?u8 {
        if (self.pos >= self.input.len) return null;
        return self.input[self.pos];
    }

    /// Check if we're at end of input
    fn atEnd(self: *const Interpreter) bool {
        return self.pos >= self.input.len;
    }

    /// Advance position by one character
    fn advance(self: *Interpreter) !void {
        if (self.pos >= self.input.len) return ParseError.EndOfStream;

        if (self.input[self.pos] == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.pos += 1;
    }

    /// Save current position for backtracking
    fn savePos(self: *const Interpreter) struct { pos: usize, line: usize, column: usize } {
        return .{ .pos = self.pos, .line = self.line, .column = self.column };
    }

    /// Restore position after failed match
    fn restorePos(self: *Interpreter, saved: anytype) void {
        self.pos = saved.pos;
        self.line = saved.line;
        self.column = saved.column;
    }

    /// Register an action function for a rule
    pub fn registerFunc(self: *Interpreter, name: []const u8, func: *const fn (*Current, []const Value) anyerror!Value) !void {
        try self.rule_funcs.put(name, func);
    }

    /// Parse starting from the given rule
    pub fn parse(self: *Interpreter, rule_name: []const u8) !Value {
        // Find the rule
        const rule = for (self.grammar.rules.items) |r| {
            if (std.mem.eql(u8, r.name.value, rule_name)) {
                break r;
            }
        } else {
            std.log.err("Rule not found: {s}\n", .{rule_name});
            return ParseError.RuleNotFound;
        };

        return self.execExpr(rule.expr);
    }

    /// Execute an expression and return the result
    pub fn execExpr(self: *Interpreter, expr: *const ast.Expression) !Value {
        switch (expr.*) {
            .seq => |seq| return self.execSeq(seq),
            .choice => |choice| return self.execChoice(choice),
            .action => |action| return self.execAction(action),
            .labeled => |labeled| return self.execLabeled(labeled),
            .rule_ref => |ref| return self.execRuleRef(ref),
            .lit_matcher => |lit| return self.execLitMatcher(lit),
            .char_class_matcher => |class| return self.execCharClassMatcher(class),
            .any_matcher => |any| return self.execAnyMatcher(any),
            .and_expr => |and_expr| return self.execAndExpr(and_expr),
            .not => |not| return self.execNotExpr(not),
            .zero_or_one => |z| return self.execZeroOrOne(z),
            .zero_or_more => |z| return self.execZeroOrMore(z),
            .one_or_more => |o| return self.execOneOrMore(o),
            else => {
                std.log.err("execExpr: unimplemented expression type\n", .{});
                return error.UnimplementedExpression;
            },
        }
    }

    fn execSeq(self: *Interpreter, seq: *const ast.SeqExpr) !Value {
        var results = std.ArrayList(Value).init(self.allocator);
        errdefer {
            for (results.items) |item| {
                // Cleanup if needed
                _ = item;
            }
            results.deinit(self.allocator);
        }

        const saved = self.savePos();
        errdefer self.restorePos(saved);

        for (seq.exprs.items) |sub_expr| {
            const val = try self.execExpr(sub_expr);
            try results.append(val);
        }

        return Value{ .list = results };
    }

    fn execChoice(self: *Interpreter, choice: *const ast.ChoiceExpr) !Value {
        const saved = self.savePos();

        for (choice.alternatives.items) |alt| {
            if (self.execExpr(alt)) |val| {
                // Success! Don't restore position
                return val;
            } else |_| {
                // Failed, restore position and try next
                self.restorePos(saved);
                continue;
            }
        }

        return ParseError.NoMatch;
    }

    fn execAction(self: *Interpreter, action: *const ast.ActionExpr) !Value {
        // Execute the sub-expression first
        const val = try self.execExpr(action.expr);

        // Push to value stack
        try self.vstack.append(val);

        // TODO: Call the registered action function
        // For now, just return the value
        return val;
    }

    fn execLabeled(self: *Interpreter, labeled: *const ast.LabeledExpr) !Value {
        // Just execute the expression, label is metadata for actions
        return self.execExpr(labeled.expr);
    }

    fn execRuleRef(self: *Interpreter, ref: *const ast.RuleRefExpr) !Value {
        // Find the rule in the grammar
        for (self.grammar.rules.items) |rule| {
            if (std.mem.eql(u8, rule.name.value, ref.name.value)) {
                // Execute the rule's expression
                return self.execExpr(rule.expr);
            }
        }
        return ParseError.RuleNotFound;
    }

    fn execLitMatcher(self: *Interpreter, lit: *const ast.LitMatcher) !Value {
        const saved = self.savePos();
        errdefer self.restorePos(saved);

        // Check if the literal matches at current position
        if (self.pos + lit.value.len > self.input.len) {
            return ParseError.NoMatch;
        }

        const slice = self.input[self.pos..][0..lit.value.len];
        if (!std.mem.eql(u8, slice, lit.value)) {
            return ParseError.NoMatch;
        }

        // Match! Advance position
        for (0..lit.value.len) |_| {
            try self.advance();
        }

        // Update current context
        self.cur.text_buf = slice;
        self.cur.pos = ast.Pos{
            .line = saved.line,
            .column = saved.column,
            .offset = @intCast(saved.pos),
        };

        return Value{ .string = slice };
    }

    fn execCharClassMatcher(self: *Interpreter, class: *const ast.CharClassMatcher) !Value {
        const saved = self.savePos();
        errdefer self.restorePos(saved);

        const char = self.currentChar() orelse return ParseError.NoMatch;

        // Check if char matches the class
        const matches = blk: {
            if (class.negated) {
                break :blk !self.inCharClass(char, class);
            } else {
                break :blk self.inCharClass(char, class);
            }
        };

        if (!matches) {
            return ParseError.NoMatch;
        }

        // Match! Advance position
        try self.advance();

        // Update current context
        self.cur.text_buf = self.input[self.pos - 1 .. self.pos];
        self.cur.pos = ast.Pos{
            .line = saved.line,
            .column = saved.column,
            .offset = @intCast(saved.pos),
        };

        return Value{ .string = self.input[self.pos - 1 .. self.pos] };
    }

    fn inCharClass(self: *const Interpreter, char: u8, class: *const ast.CharClassMatcher) bool {
        _ = self;
        for (class.classes.items) |c| {
            switch (c) {
                .char_range => |range| {
                    if (char >= range.start and char <= range.end) {
                        return true;
                    }
                },
                .char_class => |cc| {
                    if (char == cc.char) {
                        return true;
                    }
                },
            }
        }
        return false;
    }

    fn execAnyMatcher(self: *Interpreter, any: *const ast.AnyMatcher) !Value {
        _ = any;
        const saved = self.savePos();
        errdefer self.restorePos(saved);

        if (self.atEnd()) {
            return ParseError.NoMatch;
        }

        // Match any single character
        try self.advance();

        // Update current context
        self.cur.text_buf = self.input[self.pos - 1 .. self.pos];
        self.cur.pos = ast.Pos{
            .line = saved.line,
            .column = saved.column,
            .offset = @intCast(saved.pos),
        };

        return Value{ .string = self.input[self.pos - 1 .. self.pos] };
    }

    fn execAndExpr(self: *Interpreter, and_expr: *const ast.AndExpr) !Value {
        const saved = self.savePos();
        defer self.restorePos(saved);

        // Just check if expression matches, don't consume input
        _ = try self.execExpr(and_expr.expr);
        return Value{ .boolean = true };
    }

    fn execNotExpr(self: *Interpreter, not_expr: *const ast.NotExpr) !Value {
        const saved = self.savePos();
        defer self.restorePos(saved);

        // Check if expression matches
        if (self.execExpr(not_expr.expr)) {
            return ParseError.NoMatch;  // If it matches, NOT fails
        } else |_| {
            return Value{ .boolean = true };  // If it fails, NOT succeeds
        }
    }

    fn execZeroOrOne(self: *Interpreter, z: *const ast.ZeroOrOneExpr) !Value {
        var results = std.ArrayList(Value).init(self.allocator);
        errdefer {
            for (results.items) |item| {
                _ = item;
            }
            results.deinit(self.allocator);
        }

        // Try to match the expression
        if (self.execExpr(z.expr)) |val| {
            try results.append(val);
        } else |_| {
            // Optional: return empty list even if no match
        }

        return Value{ .list = results };
    }

    fn execZeroOrMore(self: *Interpreter, z: *const ast.ZeroOrMoreExpr) !Value {
        var results = std.ArrayList(Value).init(self.allocator);
        errdefer {
            for (results.items) |item| {
                _ = item;
            }
            results.deinit(self.allocator);
        }

        // Match zero or more times
        while (true) {
            if (self.execExpr(z.expr)) |val| {
                try results.append(val);
            } else |_| {
                break;
            }
        }

        return Value{ .list = results };
    }

    fn execOneOrMore(self: *Interpreter, o: *const ast.OneOrMoreExpr) !Value {
        var results = std.ArrayList(Value).init(self.allocator);
        errdefer {
            for (results.items) |item| {
                _ = item;
            }
            results.deinit(self.allocator);
        }

        // Must match at least once
        const first = try self.execExpr(o.expr);
        try results.append(first);

        // Then match zero or more times
        while (true) {
            if (self.execExpr(o.expr)) |val| {
                try results.append(val);
            } else |_| {
                break;
            }
        }

        return Value{ .list = results };
    }
};
