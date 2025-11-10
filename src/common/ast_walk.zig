const std = @import("std");
const ast = @import("ast.zig");

/// A Visitor implements a visit method, which is invoked for each Expression
/// encountered by walk.
/// If the result visitor w is not null, walk visits each of the children
/// of Expression with the visitor w, followed by a call of w.visit(null).
pub const Visitor = struct {
    visitFn: *const fn (visitor: *Visitor, expr: ?*ast.Expression) ?*Visitor,

    pub fn visit(self: *Visitor, expr: ?*ast.Expression) ?*Visitor {
        return self.visitFn(self, expr);
    }
};

/// walk traverses an AST in depth-first order: It starts by calling
/// v.visit(expr); Expression must not be null. If the visitor w returned by
/// v.visit(expr) is not null, walk is invoked recursively with visitor
/// w for each of the non-nil children of Expression, followed by a call of
/// w.visit(null).
pub fn walk(v: *Visitor, expr: *ast.Expression) void {
    const new_visitor = v.visit(expr) orelse return;

    // Visit children based on the expression type
    switch (expr.*) {
        .action => |*action_expr| {
            walk(new_visitor, &action_expr.expr);
        },
        .and_code => {
            // Nothing to do - no children to visit
        },
        .and_expr => |*and_expr| {
            walk(new_visitor, &and_expr.expr);
        },
        .any_matcher => {
            // Nothing to do - no children to visit
        },
        .char_class_matcher => {
            // Nothing to do - no children to visit
        },
        .choice => |*choice_expr| {
            for (choice_expr.alternatives.items) |*alt| {
                walk(new_visitor, alt);
            }
        },
        .recovery => |*recovery_expr| {
            walk(new_visitor, &recovery_expr.expr);
            walk(new_visitor, &recovery_expr.recover_expr);
        },
        .seq => |*seq_expr| {
            for (seq_expr.exprs.items) |*e| {
                walk(new_visitor, e);
            }
        },
        .labeled => |*labeled_expr| {
            walk(new_visitor, &labeled_expr.expr);
        },
        .lit_matcher => {
            // Nothing to do - no children to visit
        },
        .not_code => {
            // Nothing to do - no children to visit
        },
        .not => |*not_expr| {
            walk(new_visitor, &not_expr.expr);
        },
        .one_or_more => |*one_or_more_expr| {
            walk(new_visitor, &one_or_more_expr.expr);
        },
        .zero_or_more => |*zero_or_more_expr| {
            walk(new_visitor, &zero_or_more_expr.expr);
        },
        .zero_or_one => |*zero_or_one_expr| {
            walk(new_visitor, &zero_or_one_expr.expr);
        },
        .rule_ref => {
            // Nothing to do - no children to visit
        },
        .state_code => {
            // Nothing to do - no children to visit
        },
        .throw => {
            // Nothing to do - no children to visit
        },
    }

    // Final call with null expression
    _ = new_visitor.visit(null);
}

/// Inspector function type for the inspect function
pub const InspectorFn = *const fn (expr: *ast.Expression) bool;

/// Inspector implements Visitor interface for function-based inspection
const Inspector = struct {
    fn_ptr: InspectorFn,
    visitor: Visitor,

    fn init(fn_ptr: InspectorFn) Inspector {
        return .{
            .fn_ptr = fn_ptr,
            .visitor = .{ .visitFn = visit },
        };
    }

    fn visit(visitor: *Visitor, expr: ?*ast.Expression) ?*Visitor {
        const self: *Inspector = @fieldParentPtr("visitor", visitor);
        if (expr) |e| {
            if (self.fn_ptr(e)) {
                return &self.visitor;
            }
        }
        return null;
    }
};

/// inspect traverses an AST in depth-first order: It starts by calling
/// f(expr); expr must not be null. If f returns true, inspect invokes f
/// recursively for each of the non-nil children of expr, followed by a
/// call of f(null).
pub fn inspect(expr: *ast.Expression, f: InspectorFn) void {
    var inspector = Inspector.init(f);
    walk(&inspector.visitor, expr);
}
