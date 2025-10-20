const std = @import("std");

// Universal AST node types that work for both EBNF and PEGZ grammars

// Grammar format types
pub const GrammarFormat = enum {
    ebnf,
    pegz,
};

// Forward declarations
pub const ActionExpr = struct {
    sequence: SeqExpr,
    action_code: ?[]const u8 = null,

    pub fn init(seq: SeqExpr) ActionExpr {
        return ActionExpr{ .sequence = seq };
    }

    pub fn withAction(self: ActionExpr, code: []const u8) ActionExpr {
        return ActionExpr{
            .sequence = self.sequence,
            .action_code = code,
        };
    }

    pub fn deinit(self: *ActionExpr) void {
        _ = self;
    }
};

pub const SeqExpr = struct {
    expressions: std.ArrayList(LabeledExpr),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SeqExpr {
        return SeqExpr{
            .expressions = std.ArrayList(LabeledExpr).initCapacity(allocator, 0) catch unreachable,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SeqExpr) void {
        for (self.expressions.items) |*expr| {
            expr.deinit();
        }
        self.expressions.deinit();
    }

    pub fn addExpression(self: *SeqExpr, expr: LabeledExpr) !void {
        try self.expressions.append(expr);
    }
};

pub const LabeledExpr = struct {
    label: ?[]const u8 = null,
    expression: PrefixedExpr,

    pub fn init(expr: PrefixedExpr) LabeledExpr {
        return LabeledExpr{ .expression = expr };
    }

    pub fn withLabel(self: LabeledExpr, label: []const u8) LabeledExpr {
        return LabeledExpr{
            .label = label,
            .expression = self.expression,
        };
    }

    pub fn deinit(self: *LabeledExpr) void {
        self.expression.deinit();
    }
};

pub const PrefixedOp = enum {
    and_op,    // ampersand &
    not_op,    // exclamation !
};

pub const PrefixedExpr = struct {
    op: ?PrefixedOp = null,
    expression: SuffixedExpr,

    pub fn init(expr: SuffixedExpr) PrefixedExpr {
        return PrefixedExpr{ .expression = expr };
    }

    pub fn withOp(self: PrefixedExpr, op: PrefixedOp) PrefixedExpr {
        return PrefixedExpr{
            .op = op,
            .expression = self.expression,
        };
    }

    pub fn deinit(self: *PrefixedExpr) void {
        self.expression.deinit();
    }
};

pub const SuffixedOp = enum {
    optional,  // question ?
    zero_or_more, // star *
    one_or_more,  // plus +
};

pub const SuffixedExpr = struct {
    expression: PrimaryExpr,
    op: ?SuffixedOp = null,

    pub fn init(expr: PrimaryExpr) SuffixedExpr {
        return SuffixedExpr{ .expression = expr };
    }

    pub fn withOp(self: SuffixedExpr, op: SuffixedOp) SuffixedExpr {
        return SuffixedExpr{
            .expression = self.expression,
            .op = op,
        };
    }

    pub fn deinit(self: *SuffixedExpr) void {
        self.expression.deinit();
    }
};

pub const PrimaryExpr = union(enum) {
    literal: LiteralMatcher,
    char_class: CharClassMatcher,
    any_matcher: AnyMatcher,
    rule_ref: RuleRefExpr,
    grouped: GroupedExpr,

    pub fn deinit(self: *PrimaryExpr) void {
        switch (self.*) {
            .literal => |*l| l.deinit(),
            .char_class => |*c| c.deinit(),
            .any_matcher => {},
            .rule_ref => |*r| r.deinit(),
            .grouped => |*g| g.deinit(),
        }
    }
};

pub const LiteralMatcher = struct {
    value: []const u8,
    literal_type: LiteralType,

    pub const LiteralType = enum {
        string,    // regular string "text"
        raw_string, // raw string r"text"
        character, // character 'c'
    };

    pub fn init(value: []const u8, literal_type: LiteralType) LiteralMatcher {
        return LiteralMatcher{ .value = value, .literal_type = literal_type };
    }

    pub fn deinit(self: *LiteralMatcher) void {
        _ = self;
        // Value is typically owned by the lexer, no need to free here
    }
};

pub const CharClassMatcher = struct {
    class_definition: []const u8,

    pub fn init(class_def: []const u8) CharClassMatcher {
        return CharClassMatcher{ .class_definition = class_def };
    }

    pub fn deinit(self: *CharClassMatcher) void {
        _ = self;
        // Class definition is owned by lexer
    }
};

pub const AnyMatcher = struct {
    pub fn init() AnyMatcher {
        return AnyMatcher{};
    }
};

pub const RuleRefExpr = struct {
    rule_name: []const u8,

    pub fn init(name: []const u8) RuleRefExpr {
        return RuleRefExpr{ .rule_name = name };
    }

    pub fn deinit(self: *RuleRefExpr) void {
        _ = self;
        // Rule name is owned by lexer
    }
};

pub const GroupedExpr = struct {
    expression: *Expression,

    pub fn init(expr: *Expression) GroupedExpr {
        return GroupedExpr{ .expression = expr };
    }

    pub fn deinit(self: *GroupedExpr) void {
        _ = self;
        // Expression cleanup handled by owner
    }
};

// Base expression node type
pub const Expression = union(enum) {
    choice: ChoiceExpr,
    action: ActionExpr,
    sequence: SeqExpr,
    labeled: LabeledExpr,
    prefixed: PrefixedExpr,
    suffixed: SuffixedExpr,
    primary: PrimaryExpr,

    pub fn format(self: Expression, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .choice => |c| try writer.print("Choice{}", .{c}),
            .action => |a| try writer.print("Action{}", .{a}),
            .sequence => |s| try writer.print("Seq{}", .{s}),
            .labeled => |l| try writer.print("Labeled{}", .{l}),
            .prefixed => |p| try writer.print("Prefixed{}", .{p}),
            .suffixed => |s| try writer.print("Suffixed{}", .{s}),
            .primary => |p| try writer.print("Primary{}", .{p}),
        }
    }

    pub fn deinit(self: *Expression) void {
        switch (self.*) {
            .choice => |*c| {
                for (c.alternatives.items) |*alt| {
                    alt.deinit();
                }
                c.alternatives.deinit();
            },
            .action => |*a| {
                a.sequence.deinit();
            },
            .sequence => |*s| {
                s.deinit();
            },
            .labeled => |*l| {
                l.deinit();
            },
            .prefixed => |*p| {
                p.deinit();
            },
            .suffixed => |*s| {
                s.deinit();
            },
            .primary => |*p| {
                p.deinit();
            },
        }
    }
};

pub const ChoiceExpr = struct {
    alternatives: std.ArrayList(ActionExpr),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ChoiceExpr {
        return ChoiceExpr{
            .alternatives = std.ArrayList(ActionExpr).initCapacity(allocator, 0) catch unreachable,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ChoiceExpr) void {
        self.alternatives.deinit();
    }

    pub fn addAlternative(self: *ChoiceExpr, expr: ActionExpr) !void {
        try self.alternatives.append(expr);
    }
};

// Rule and Grammar structures
pub const Rule = struct {
    name: []const u8,
    display_name: ?[]const u8 = null, // for str | rstr | char after rule name (EBNF specific)
    expression: Expression,
    attributes: ?std.json.Value = null, // For future PEGZ extensions
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, expr: Expression) Rule {
        return Rule{
            .name = name,
            .expression = expr,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Rule) void {
        self.expression.deinit();
    }

    pub fn withDisplayName(self: Rule, display: []const u8) Rule {
        return Rule{
            .name = self.name,
            .display_name = display,
            .expression = self.expression,
            .allocator = self.allocator,
        };
    }

    pub fn withAttributes(self: Rule, attrs: std.json.Value) Rule {
        return Rule{
            .name = self.name,
            .display_name = self.display_name,
            .expression = self.expression,
            .attributes = attrs,
            .allocator = self.allocator,
        };
    }
};

pub const Grammar = struct {
    preamble: ?Preamble = null,
    rules: std.ArrayList(Rule),
    format: GrammarFormat,
    allocator: std.mem.Allocator,

    pub const Preamble = struct {
        code: ?[]const u8 = null,
        imports: ?[][]const u8 = null,
        other_sections: ?std.json.ObjectMap = null,
    };

    pub fn init(allocator: std.mem.Allocator, format: GrammarFormat) Grammar {
        return Grammar{
            .rules = std.ArrayList(Rule).initCapacity(allocator, 0) catch unreachable,
            .format = format,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Grammar) void {
        for (self.rules.items) |*rule| {
            rule.deinit();
        }
        self.rules.deinit(self.allocator);
    }

    pub fn addRule(self: *Grammar, rule: Rule) !void {
        try self.rules.append(rule);
    }

    pub fn withPreamble(self: *Grammar, preamble: Preamble) void {
        self.preamble = preamble;
    }

    pub fn findRule(self: Grammar, name: []const u8) ?*Rule {
        for (self.rules.items) |*rule| {
            if (std.mem.eql(u8, rule.name, name)) {
                return rule;
            }
        }
        return null;
    }

    pub fn getStartRule(self: Grammar) ?*Rule {
        if (self.rules.items.len > 0) {
            return &self.rules.items[0];
        }
        return null;
    }
};

// Expression cleanup function
pub fn deinitExpression(expr: *Expression, allocator_inner: std.mem.Allocator) void {
        _ = allocator_inner;
    expr.deinit();
}

// Utility functions for working with expressions
pub fn cloneExpression(expr: Expression, allocator: std.mem.Allocator) !Expression {
    switch (expr) {
        .choice => |c| {
            var new_choice = ChoiceExpr.init(allocator);
            for (c.alternatives.items) |alt| {
                const cloned_alt = try cloneActionExpr(alt, allocator);
                try new_choice.addAlternative(cloned_alt);
            }
            return Expression{ .choice = new_choice };
        },
        .action => |a| {
            const cloned_seq = try cloneSeqExpr(a.sequence, allocator);
            var new_action = ActionExpr.init(cloned_seq);
            if (a.action_code) |code| {
                new_action.action_code = try allocator.dupe(u8, code);
            }
            return Expression{ .action = new_action };
        },
        .sequence => |s| {
            const cloned_seq = try cloneSeqExpr(s, allocator);
            return Expression{ .sequence = cloned_seq };
        },
        .labeled => |l| {
            const cloned_prefixed = try clonePrefixedExpr(l.expression, allocator);
            var new_labeled = LabeledExpr.init(cloned_prefixed);
            if (l.label) |label| {
                new_labeled.label = try allocator.dupe(u8, label);
            }
            return Expression{ .labeled = new_labeled };
        },
        .prefixed => |p| {
            const cloned_suffixed = try cloneSuffixedExpr(p.expression, allocator);
            var new_prefixed = PrefixedExpr.init(cloned_suffixed);
            new_prefixed.op = p.op;
            return Expression{ .prefixed = new_prefixed };
        },
        .suffixed => |s| {
            const cloned_primary = try clonePrimaryExpr(s.expression, allocator);
            var new_suffixed = SuffixedExpr.init(cloned_primary);
            new_suffixed.op = s.op;
            return Expression{ .suffixed = new_suffixed };
        },
        .primary => |p| {
            const cloned_primary = try clonePrimaryExpr(p, allocator);
            return Expression{ .primary = cloned_primary };
        },
    }
}

fn cloneActionExpr(expr: ActionExpr, allocator: std.mem.Allocator) !ActionExpr {
    const cloned_seq = try cloneSeqExpr(expr.sequence, allocator);
    var new_action = ActionExpr.init(cloned_seq);
    if (expr.action_code) |code| {
        new_action.action_code = try allocator.dupe(u8, code);
    }
    return new_action;
}

fn cloneSeqExpr(expr: SeqExpr, allocator: std.mem.Allocator) !SeqExpr {
    var new_seq = SeqExpr.init(allocator);
    for (expr.expressions.items) |labeled_expr| {
        const cloned_labeled = try cloneLabeledExpr(labeled_expr, allocator);
        try new_seq.addExpression(cloned_labeled);
    }
    return new_seq;
}

fn cloneLabeledExpr(expr: LabeledExpr, allocator: std.mem.Allocator) !LabeledExpr {
    const cloned_prefixed = try clonePrefixedExpr(expr.expression, allocator);
    var new_labeled = LabeledExpr.init(cloned_prefixed);
    if (expr.label) |label| {
        new_labeled.label = try allocator.dupe(u8, label);
    }
    return new_labeled;
}

fn clonePrefixedExpr(expr: PrefixedExpr, allocator: std.mem.Allocator) !PrefixedExpr {
    const cloned_suffixed = try cloneSuffixedExpr(expr.expression, allocator);
    var new_prefixed = PrefixedExpr.init(cloned_suffixed);
    new_prefixed.op = expr.op;
    return new_prefixed;
}

fn cloneSuffixedExpr(expr: SuffixedExpr, allocator: std.mem.Allocator) !SuffixedExpr {
    const cloned_primary = try clonePrimaryExpr(expr.expression, allocator);
    var new_suffixed = SuffixedExpr.init(cloned_primary);
    new_suffixed.op = expr.op;
    return new_suffixed;
}

fn clonePrimaryExpr(expr: PrimaryExpr, allocator: std.mem.Allocator) !PrimaryExpr {
    switch (expr) {
        .literal => |l| {
            const cloned_value = try allocator.dupe(u8, l.value);
            return PrimaryExpr{
                .literal = LiteralMatcher.init(cloned_value, l.literal_type),
            };
        },
        .char_class => |c| {
            const cloned_class = try allocator.dupe(u8, c.class_definition);
            return PrimaryExpr{
                .char_class = CharClassMatcher.init(cloned_class),
            };
        },
        .any_matcher => {
            return PrimaryExpr{ .any_matcher = AnyMatcher.init() };
        },
        .rule_ref => |r| {
            const cloned_name = try allocator.dupe(u8, r.rule_name);
            return PrimaryExpr{
                .rule_ref = RuleRefExpr.init(cloned_name),
            };
        },
        .grouped => |g| {
            const cloned_expr = try cloneExpression(g.expression.*, allocator);
            const expr_ptr = try allocator.create(Expression);
            expr_ptr.* = cloned_expr;
            return PrimaryExpr{
                .grouped = GroupedExpr.init(expr_ptr),
            };
        },
    }
}