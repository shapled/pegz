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
};

pub const CodeBlock = PosValue;

pub const Grammar = struct {
    pos: Pos,
    init: ?*CodeBlock,
    rules: ArrayList(*Rule),

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Init: {}, Rules: [\n", .{
            self.pos,
            @typeName(Grammar),
            if (self.init) |init| init else "null"
        });

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
    expression: Expression,
    expr: *Expression,

    visited: bool,
    nullable: bool,
    left_recursive: bool,
    leader: bool,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Name: {}, DisplayName: {}, Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.name,
            self.display_name,
            self.expr,
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
        self.nullable = self.expression.nullableVisit(rules);
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
    expr: *Expression,
    recover_expr: *Expression,
    labels: ArrayList([]const u8),
    nullable: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !*Self {
        var self = try allocator.create(Self);
        self.* = .{
            .pos = .{ .line = 0, .column = 0, .offset = 0 },
            .expr = undefined,
            .recover_expr = undefined,
            .labels = .empty,
            .nullable = false,
        };
        self.labels = ArrayList([]const u8).init(allocator);
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit();
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{}: {s}{{Expr: {}, RecoverExpr: {}, Labels: [\n", .{
            self.p,
            @typeName(RecoveryExpr),
            self.expr,
            self.recover_expr
        });

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
    expr: *Expression,
    code: *CodeBlock,
    func_ix: usize,
    nullable: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, pos: Pos, expr: *Expression, code: *CodeBlock, func_ix: usize) !*Self {
        const self = try allocator.create(Self);
        self.* = .{
            .pos = pos,
            .expr = expr,
            .code = code,
            .func_ix = func_ix,
            .nullable = false,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}, Code: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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

    pub fn init(allocator: std.mem.Allocator, pos: Pos, label: []const u8) !*Self {
        const self = try allocator.create(Self);
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
            self.p,
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
    exprs: ArrayList(*Expression),
    nullable: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, pos: Pos) !*Self {
        var self = try allocator.create(Self);
        self.* = .{
            .pos = pos,
            .exprs = .empty,
            .nullable = false,
        };
        self.exprs = ArrayList(*Expression).init(allocator);
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.exprs.deinit();
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Exprs: [\n", .{
            self.p,
            @typeName(SeqExpr)
        });

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
        for (self.exprs) |item| {
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
        for (self.exprs) |item| {
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
    label: *Identifier,
    expr: *Expression,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Label: {}, Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.label,
            self.expr,
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
    expr: *Expression,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, pos: Pos, expr: *Expression) !*Self {
        const self = try allocator.create(Self);
        self.* = .{
            .pos = pos,
            .expr = expr,
        };
        return self;
    }

    pub fn deinit(self: *Self) void {
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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
    expr: *Expression,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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
    expr: *Expression,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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
    expr: *Expression,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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
    expr: *Expression,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Expr: {}}}", .{
            self.p,
            @typeName(@TypeOf(self)),
            self.expr,
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
    name: *Identifier,
    nullable: bool,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Name: {}}}", .{
            self.p,
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
    code: *CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.p,
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
    code: *CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.p,
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
    code: *CodeBlock,
    func_ix: usize,

    const Self = @This();

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Code: {}}}", .{
            self.p,
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

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}', IgnoreCase: {}}}", .{
            self.p,
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

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}', IgnoreCase: {}, Inverted: {}}}", .{
            self.p,
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

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Val: '{s}'}}", .{
            self.p,
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
    alternatives: ArrayList(*Expression),
    nullable: bool,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, pos: Pos) !*Self {
        var self = try allocator.create(Self);
        self.* = .{
            .pos = pos,
            .alternatives = .empty,
            .nullable = false,
        };
        self.alternatives = ArrayList(*Expression).init(allocator);
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.alternatives.deinit();
        std.heap.page_allocator.destroy(self);
    }

    pub fn format(self: *const Self, writer: anytype) !void {
        try writer.print("{s}: {s}{{Alternatives: [\n", .{
            self.p,
            @typeName(ChoiceExpr)
        });

        for (self.alternatives) |alt| {
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
        for (self.alternatives) |alt| {
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
        for (self.alternatives) |alt| {
            var alt_names = try alt.initialNames(gpa);
            var it = alt_names.iterator();
            while (it.next()) |entry| {
                try names.put(entry.key_ptr.*, {});
            }
        }
        return names;
    }
};
