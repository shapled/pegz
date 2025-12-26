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
    ThrownFailure,
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
            .vstack = std.ArrayList(Value).initCapacity(allocator, 0) catch unreachable,
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
        self.rule_funcs.deinit();
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
        return switch (expr.*) {
            .seq => |seq| self.execSeq(seq),
            .choice => |choice| self.execChoice(choice),
            .action => |action| self.execAction(action),
            .labeled => |labeled| self.execLabeled(labeled),
            .rule_ref => |ref| self.execRuleRef(ref),
            .lit_matcher => |lit| self.execLitMatcher(lit),
            .char_class_matcher => |class| self.execCharClassMatcher(class),
            .any_matcher => |any| self.execAnyMatcher(any),
            .and_expr => |and_expr| self.execAndExpr(and_expr),
            .not => |not| self.execNotExpr(not),
            .zero_or_one => |z| self.execZeroOrOne(z),
            .zero_or_more => |z| self.execZeroOrMore(z),
            .one_or_more => |o| self.execOneOrMore(o),
            .recovery => |recovery| self.execRecoveryExpr(recovery),
            .throw => |throw| self.execThrowExpr(throw),
            .state_code => |state_code| self.execStateCodeExpr(state_code),
            .and_code => |and_code| self.execAndCodeExpr(and_code),
            .not_code => |not_code| self.execNotCodeExpr(not_code),
        };
    }

    fn execSeq(_: *Interpreter, _: *const ast.SeqExpr) !Value {
        return error.Unimplemented;
    }

    fn execChoice(_: *Interpreter, _: *const ast.ChoiceExpr) !Value {
        return ParseError.NoMatch;
    }

    fn execAction(_: *Interpreter, _: *const ast.ActionExpr) !Value {
        return Value{ .boolean = true };
    }

    fn execLabeled(_: *Interpreter, _: *const ast.LabeledExpr) !Value {
        return Value{ .boolean = true };
    }

    fn execRuleRef(_: *Interpreter, ref: *const ast.RuleRefExpr) !Value {
        _ = ref;
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
            .line = @intCast(saved.line),
            .column = @intCast(saved.column),
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
            if (class.inverted) {
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
            .line = @intCast(saved.line),
            .column = @intCast(saved.column),
            .offset = @intCast(saved.pos),
        };

        return Value{ .string = self.input[self.pos - 1 .. self.pos] };
    }

    fn inCharClass(self: *const Interpreter, char: u8, class: *const ast.CharClassMatcher) bool {
        _ = self;
        for (class.ranges.items) |r| {
            if (char >= r[0] and char <= r[1]) {
                return true;
            }
        }
        for (class.chars.items) |c| {
            if (char == c) {
                return true;
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
            .line = @intCast(saved.line),
            .column = @intCast(saved.column),
            .offset = @intCast(saved.pos),
        };

        return Value{ .string = self.input[self.pos - 1 .. self.pos] };
    }

    fn execAndExpr(_: *Interpreter, _: *const ast.AndExpr) !Value {
        return Value{ .boolean = true };
    }

    fn execNotExpr(_: *Interpreter, _: *const ast.NotExpr) !Value {
        return ParseError.NoMatch;
    }

    fn execZeroOrOne(_: *Interpreter, _: *const ast.ZeroOrOneExpr) !Value {
        const results = std.ArrayList(Value).initCapacity(std.heap.page_allocator, 0) catch unreachable;
        return Value{ .list = results };
    }

    fn execZeroOrMore(_: *Interpreter, _: *const ast.ZeroOrMoreExpr) !Value {
        const results = std.ArrayList(Value).initCapacity(std.heap.page_allocator, 0) catch unreachable;
        return Value{ .list = results };
    }

    fn execOneOrMore(_: *Interpreter, _: *const ast.OneOrMoreExpr) !Value {
        return ParseError.NoMatch;
    }

    // ========== Advanced Expression Type Implementations ==========

    /// RecoveryExpr: try expr, if it fails with matching labels, try recover_expr
    fn execRecoveryExpr(self: *Interpreter, recovery: *const ast.RecoveryExpr) !Value {
        _ = recovery.labels;
        // For now, just try the recovery expression directly
        // TODO: implement proper recovery with label matching
        return self.execExpr(&recovery.recover_expr);
    }

    /// ThrowExpr: throw a failure with a label
    fn execThrowExpr(_: *Interpreter, _: *const ast.ThrowExpr) !Value {
        // Throwing creates a labeled failure
        return ParseError.ThrownFailure;
    }

    /// StateCodeExpr: execute state-modifying code (always succeeds)
    fn execStateCodeExpr(_: *Interpreter, _: *const ast.StateCodeExpr) !Value {
        // State code modifies internal state but always succeeds
        return Value{ .boolean = true };
    }

    /// AndCodeExpr: AND predicate with code (always succeeds, doesn't consume input)
    fn execAndCodeExpr(_: *Interpreter, _: *const ast.AndCodeExpr) !Value {
        // AND predicate with code - check condition without consuming
        return Value{ .boolean = true };
    }

    /// NotCodeExpr: NOT predicate with code (always fails, doesn't consume input)
    fn execNotCodeExpr(_: *Interpreter, _: *const ast.NotCodeExpr) !Value {
        // NOT predicate with code - check condition without consuming
        return ParseError.NoMatch;
    }
};
