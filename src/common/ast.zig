const std = @import("std");

const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

// Universal AST node types that work for both EBNF and PEGZ grammars

pub const Pos = struct {
    filename: ?[]const u8 = null,
    line: u32,
    column: u32,
    offset: u32,

    const Self = @This();

    pub fn init(line: u32, column: u32, offset: u32) Pos {
        return .{ .line = line, .column = column, .offset = offset };
    }

    pub fn initFile(filename: []const u8, line: u32, column: u32, offset: u32) Pos {
        return .{ .filename = filename, .line = line, .column = column, .offset = offset };
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        if (self.filename != null) {
            try writer.print("{s}:{d}:{d} ({d})", .{ self.filename.?, self.line, self.column, self.offset });
        } else {
            try writer.print("{d}:{d} ({d})", .{ self.line, self.column, self.offset });
        }
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }
};

const PosValue = struct {
    pos: Pos,
    value: []const u8,

    pub fn init(pos: Pos, value: []const u8) PosValue {
        return .{ .pos = pos, .value = value };
    }
};

pub const CodeBlock = PosValue;

/// FailureLabel is a label used for error recovery
pub const FailureLabel = []const u8;

pub const Grammar = struct {
    pos: Pos,
    init: ?*CodeBlock,
    rules: ArrayList(*Rule),

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, rules: ?[]const *Rule) !*Grammar {
        const self = try allocator.create(Grammar);
        self.* = .{
            .pos = pos,
            .init = null,
            .rules = ArrayList(*Rule).initCapacity(allocator, if (rules) |r| r.len else 0) catch unreachable,
        };

        // Add rules if provided
        if (rules) |rules_list| {
            for (rules_list) |rule| {
                try self.rules.append(allocator, rule);
            }
        }

        return self;
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Init: {}, Rules: [\n", .{ self.pos, @typeName(Grammar), if (self.init) |init| init else "null" });

        for (self.rules) |rule| {
            try writer.print("{},\n", .{rule});
        }

        try writer.writeAll("]}");
    }

    fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }
};

pub const Identifier = PosValue;

pub const StringLit = PosValue;

pub const Rule = struct {
    pos: Pos,
    name: Identifier,
    display_name: StringLit,
    expr: Expression,

    visited: bool,
    nullable: bool,
    left_recursive: bool,
    leader: bool,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, name: Identifier, display_name: StringLit, expr: Expression) !*Rule {
        const self = try allocator.create(Rule);
        self.* = .{
            .pos = pos,
            .name = name,
            .display_name = display_name,
            .expr = expr,
            .visited = false,
            .nullable = false,
            .left_recursive = false,
            .leader = false,
        };
        return self;
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Name: {}, DisplayName: {}, Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.name,
            self.display_name,
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        if (self.visited) {
            return false;
        }
        self.visited = true;
        self.nullable = self.expr.nullableVisit(rules);
        self.visited = false;
        return self.nullable;
    }

    pub fn isNullable(self: *Self) bool {
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const Expression = union(enum) {
    choice: *ChoiceExpr,
    recovery: *RecoveryExpr,
    action: *ActionExpr,
    throw: *ThrowExpr,
    seq: *SeqExpr,
    labeled: *LabeledExpr,
    and_expr: *AndExpr,
    not: *NotExpr,
    zero_or_one: *ZeroOrOneExpr,
    zero_or_more: *ZeroOrMoreExpr,
    one_or_more: *OneOrMoreExpr,
    rule_ref: *RuleRefExpr,
    state_code: *StateCodeExpr,
    and_code: *AndCodeExpr,
    not_code: *NotCodeExpr,
    lit_matcher: *LitMatcher,
    char_class_matcher: *CharClassMatcher,
    any_matcher: *AnyMatcher,

    const Self = @This();

    /// Initialize an Expression from any expression type pointer
    pub fn init(expr: anytype) Expression {
        // Type matching happens at compile time through monomorphization
        if (@TypeOf(expr) == *ChoiceExpr) return .{ .choice = expr };
        if (@TypeOf(expr) == *RecoveryExpr) return .{ .recovery = expr };
        if (@TypeOf(expr) == *ActionExpr) return .{ .action = expr };
        if (@TypeOf(expr) == *ThrowExpr) return .{ .throw = expr };
        if (@TypeOf(expr) == *SeqExpr) return .{ .seq = expr };
        if (@TypeOf(expr) == *LabeledExpr) return .{ .labeled = expr };
        if (@TypeOf(expr) == *AndExpr) return .{ .and_expr = expr };
        if (@TypeOf(expr) == *NotExpr) return .{ .not = expr };
        if (@TypeOf(expr) == *ZeroOrOneExpr) return .{ .zero_or_one = expr };
        if (@TypeOf(expr) == *ZeroOrMoreExpr) return .{ .zero_or_more = expr };
        if (@TypeOf(expr) == *OneOrMoreExpr) return .{ .one_or_more = expr };
        if (@TypeOf(expr) == *RuleRefExpr) return .{ .rule_ref = expr };
        if (@TypeOf(expr) == *StateCodeExpr) return .{ .state_code = expr };
        if (@TypeOf(expr) == *AndCodeExpr) return .{ .and_code = expr };
        if (@TypeOf(expr) == *NotCodeExpr) return .{ .not_code = expr };
        if (@TypeOf(expr) == *LitMatcher) return .{ .lit_matcher = expr };
        if (@TypeOf(expr) == *CharClassMatcher) return .{ .char_class_matcher = expr };
        if (@TypeOf(expr) == *AnyMatcher) return .{ .any_matcher = expr };
        @compileError("Expression.init() does not support this type");
    }

    pub fn pos(self: *const Self) Pos {
        return switch (self.*) {
            inline else => |case| {
                return case.pos;
            },
        };
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        return switch (self.*) {
            inline else => |case| {
                return case.nullableVisit(rules);
            },
        };
    }
    pub fn isNullable(self: *Self) bool {
        return switch (self.*) {
            inline else => |case| {
                return case.nullable;
            },
        };
    }
    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return switch (self.*) {
            inline else => |case| {
                return case.initialNames(gpa);
            },
        };
    }
    pub fn format(self: *const Self, writer: anytype) !void {
        try switch (self.*) {
            inline else => |case| {
                try case.format(writer);
            },
        };
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        return switch (self.*) {
            inline else => |case| {
                var buffer: std.ArrayList(u8) = .empty;
                defer buffer.deinit(gpa);
                try case.format(buffer.writer(gpa));
                return buffer.toOwnedSlice(gpa);
            },
        };
    }
};

pub const RecoveryExpr = struct {
    pos: Pos,
    expr: Expression,
    recover_expr: Expression,
    labels: ArrayList([]const u8),
    nullable: bool,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression, recover_expr: Expression, labels: []const []const u8) !*RecoveryExpr {
        const self = try allocator.create(RecoveryExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .recover_expr = recover_expr,
            .labels = ArrayList([]const u8).initCapacity(allocator, labels.len) catch unreachable,
            .nullable = false,
            .allocator = allocator,
        };
        for (labels) |label| {
            self.labels.append(allocator, label) catch unreachable;
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{}: {s}{{Expr: {}, RecoverExpr: {}, Labels: [\n", .{ self.pos, @typeName(RecoveryExpr), &self.expr, &self.recover_expr });

        for (self.labels) |label| {
            try writer.print("{s},\n", .{label});
        }

        try writer.writeAll("]}");
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        self.nullable = self.expr.nullableVisit(rules) or self.recover_expr.nullableVisit(rules);
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        var names = StringHashMap(void).init(gpa);

        var expr_names = try self.expr.initialNames(gpa);
        var it = expr_names.iterator();
        while (it.next()) |entry| {
            try names.put(entry.key_ptr.*, {});
        }

        var recover_names = try self.recover_expr.initialNames(gpa);
        it = recover_names.iterator();
        while (it.next()) |entry| {
            try names.put(entry.key_ptr.*, {});
        }

        return names;
    }
};

pub const ActionExpr = struct {
    pos: Pos,
    expr: Expression,
    code: CodeBlock,
    func_ix: usize,
    nullable: bool,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression, code: CodeBlock, func_ix: usize) !*ActionExpr {
        const self = try allocator.create(ActionExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .code = code,
            .func_ix = func_ix,
            .nullable = false,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}, Code: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
            self.code,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        self.nullable = self.expr.nullableVisit(rules);
        return self.nullable;
    }

    pub fn isNullable(self: *Self) bool {
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const ThrowExpr = struct {
    pos: Pos,
    label: []const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, label: []const u8) !*ThrowExpr {
        const self = try allocator.create(ThrowExpr);
        self.* = .{
            .pos = pos,
            .label = label,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Label: {s}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.label,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const SeqExpr = struct {
    pos: Pos,
    exprs: ArrayList(Expression),
    nullable: bool,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, exprs: []const Expression) !*SeqExpr {
        const self = try allocator.create(SeqExpr);
        self.* = .{
            .pos = pos,
            .exprs = ArrayList(Expression).initCapacity(allocator, exprs.len) catch unreachable,
            .nullable = false,
            .allocator = allocator,
        };
        for (exprs) |expr| {
            self.exprs.append(allocator, expr) catch unreachable;
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.exprs.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Exprs: [\n", .{ self.pos, @typeName(SeqExpr) });

        for (self.exprs) |expr| {
            try writer.print("{},\n", .{expr});
        }

        try writer.writeAll("]}");
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        for (self.exprs) |*item| {
            if (!item.nullableVisit(rules)) {
                self.nullable = false;
                return false;
            }
        }
        self.nullable = true;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        var names = StringHashMap(void).init(gpa);
        for (self.exprs) |*item| {
            var item_names = try item.initialNames(gpa);
            var it = item_names.iterator();
            while (it.next()) |entry| {
                try names.put(entry.key_ptr.*, {});
            }
            if (!item.isNullable()) {
                break;
            }
        }
        return names;
    }
};

pub const LabeledExpr = struct {
    pos: Pos,
    label: Identifier,
    expr: Expression,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, label: Identifier, expr: Expression) !*LabeledExpr {
        const self = try allocator.create(LabeledExpr);
        self.* = .{
            .pos = pos,
            .label = label,
            .expr = expr,
        };
        return self;
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Label: {}, Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.label,
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        return self.expr.nullableVisit(rules);
    }

    pub fn isNullable(self: *Self) bool {
        return self.expr.isNullable();
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const AndExpr = struct {
    pos: Pos,
    expr: Expression,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression) !*AndExpr {
        const self = try allocator.create(AndExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const NotExpr = struct {
    pos: Pos,
    expr: Expression,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression) !*NotExpr {
        const self = try allocator.create(NotExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const ZeroOrOneExpr = struct {
    pos: Pos,
    expr: Expression,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression) !*ZeroOrOneExpr {
        const self = try allocator.create(ZeroOrOneExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const ZeroOrMoreExpr = struct {
    pos: Pos,
    expr: Expression,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression) !*ZeroOrMoreExpr {
        const self = try allocator.create(ZeroOrMoreExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const OneOrMoreExpr = struct {
    pos: Pos,
    expr: Expression,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, expr: Expression) !*OneOrMoreExpr {
        const self = try allocator.create(OneOrMoreExpr);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            &self.expr,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return false;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return false;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        return self.expr.initialNames(gpa);
    }
};

pub const RuleRefExpr = struct {
    pos: Pos,
    name: Identifier,
    nullable: bool,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, name: Identifier) !*RuleRefExpr {
        const self = try allocator.create(RuleRefExpr);
        self.* = .{
            .pos = pos,
            .name = name,
            .nullable = false,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Name: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.name,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        if (rules.get(self.name.value)) |item| {
            self.nullable = item.nullableVisit(rules);
            return self.nullable;
        }
        // Token or unknown; never empty.
        self.nullable = false;
        return false;
    }

    pub fn isNullable(self: *Self) bool {
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        var names = StringHashMap(void).init(gpa);
        try names.put(self.name.value, {});
        return names;
    }
};

pub const StateCodeExpr = struct {
    pos: Pos,
    code: CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, code: CodeBlock, func_ix: usize) !*StateCodeExpr {
        const self = try allocator.create(StateCodeExpr);
        self.* = .{
            .pos = pos,
            .code = code,
            .func_ix = func_ix,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.code,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const AndCodeExpr = struct {
    pos: Pos,
    code: CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, code: CodeBlock, func_ix: usize) !*AndCodeExpr {
        const self = try allocator.create(AndCodeExpr);
        self.* = .{
            .pos = pos,
            .code = code,
            .func_ix = func_ix,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.code,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const NotCodeExpr = struct {
    pos: Pos,
    code: CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, code: CodeBlock, func_ix: usize) !*NotCodeExpr {
        const self = try allocator.create(NotCodeExpr);
        self.* = .{
            .pos = pos,
            .code = code,
            .func_ix = func_ix,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.code,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return true;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return true;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const LitMatcher = struct {
    pos: Pos,
    value: []const u8,
    ignore_case: bool,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, value: []const u8, ignore_case: bool) !*LitMatcher {
        const self = try allocator.create(LitMatcher);
        self.* = .{
            .pos = pos,
            .value = value,
            .ignore_case = ignore_case,
        };
        return self;
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}', IgnoreCase: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.value,
            self.ignore_case,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = rules;
        return self.isNullable();
    }

    pub fn isNullable(self: *Self) bool {
        // The string token '' is considered empty.
        return self.value.len == 0;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const CharClassMatcher = struct {
    pos: Pos,
    value: []const u8,
    ignore_case: bool,
    inverted: bool,
    chars: ArrayList(u8),
    ranges: ArrayList(struct { u8, u8 }),
    unicode_classes: ArrayList([]const u8),

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, value: []const u8, ignore_case: bool, inverted: bool, chars: []const u8, ranges: []const struct { u8, u8 }, unicode_classes: []const []const u8) !*CharClassMatcher {
        const self = try allocator.create(CharClassMatcher);
        self.* = .{
            .pos = pos,
            .value = value,
            .ignore_case = ignore_case,
            .inverted = inverted,
            .chars = ArrayList(u8).initCapacity(allocator, chars.len) catch unreachable,
            .ranges = ArrayList(struct { u8, u8 }).initCapacity(allocator, ranges.len) catch unreachable,
            .unicode_classes = ArrayList([]const u8).initCapacity(allocator, unicode_classes.len) catch unreachable,
        };
        for (chars) |c| {
            self.chars.append(allocator, c) catch unreachable;
        }
        for (ranges) |r| {
            self.ranges.append(allocator, r) catch unreachable;
        }
        for (unicode_classes) |uc| {
            self.unicode_classes.append(allocator, uc) catch unreachable;
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.chars.deinit();
        self.ranges.deinit();
        self.unicode_classes.deinit();
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}', IgnoreCase: {}, Inverted: {}}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.value,
            self.ignore_case,
            self.inverted,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = rules;
        return self.isNullable();
    }

    pub fn isNullable(self: *Self) bool {
        return self.chars.items.len == 0 and self.ranges.items.len == 0 and self.unicode_classes.items.len == 0;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const AnyMatcher = struct {
    pos: Pos,
    value: []const u8,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, value: []const u8) !*AnyMatcher {
        const self = try allocator.create(AnyMatcher);
        self.* = .{
            .pos = pos,
            .value = value,
        };
        return self;
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}'}}", .{
            self.pos,
            @typeName(@TypeOf(self)),
            self.value,
        });
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        _ = self;
        _ = rules;
        return false;
    }

    pub fn isNullable(self: *Self) bool {
        _ = self;
        return false;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        _ = self;
        return StringHashMap(void).init(gpa);
    }
};

pub const ChoiceExpr = struct {
    pos: Pos,
    alternatives: ArrayList(Expression),
    nullable: bool,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn create(allocator: std.mem.Allocator, pos: Pos, alternatives: []const Expression) !*ChoiceExpr {
        const self = try allocator.create(ChoiceExpr);
        self.* = .{
            .pos = pos,
            .alternatives = ArrayList(Expression).initCapacity(allocator, alternatives.len) catch unreachable,
            .nullable = false,
            .allocator = allocator,
        };
        for (alternatives) |alt| {
            self.alternatives.append(allocator, alt) catch unreachable;
        }
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.alternatives.deinit(self.allocator);
        self.allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Alternatives: [\n", .{ self.pos, @typeName(ChoiceExpr) });

        for (self.alternatives) |*alt| {
            try writer.print("{},\n", .{alt});
        }

        try writer.writeAll("]}}");
    }

    pub fn toString(self: *Self, gpa: std.mem.Allocator) ![]const u8 {
        var buffer: std.ArrayList(u8) = .empty;
        defer buffer.deinit(gpa);
        try self.format(buffer.writer(gpa));
        return buffer.toOwnedSlice(gpa);
    }

    pub fn nullableVisit(self: *Self, rules: StringHashMap(*Rule)) bool {
        for (self.alternatives) |*alt| {
            if (alt.nullableVisit(rules)) {
                self.nullable = true;
                return true;
            }
        }
        self.nullable = false;
        return false;
    }

    pub fn isNullable(self: *Self) bool {
        return self.nullable;
    }

    pub fn initialNames(self: *Self, gpa: std.mem.Allocator) !StringHashMap(void) {
        var names = StringHashMap(void).init(gpa);
        for (self.alternatives) |*alt| {
            var alt_names = try alt.initialNames(gpa);
            var it = alt_names.iterator();
            while (it.next()) |entry| {
                try names.put(entry.key_ptr.*, {});
            }
        }
        return names;
    }
};
