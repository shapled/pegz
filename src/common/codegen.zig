const std = @import("std");
const ast = @import("ast.zig");

// Common code generator interface that can generate Zig parsers from grammar AST
pub const CodeGenerator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    grammar: *ast.Grammar,
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator, grammar: *ast.Grammar) Self {
        return Self{
            .allocator = allocator,
            .grammar = grammar,
            .output = std.ArrayList(u8).initCapacity(allocator, 0) catch unreachable,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit();
    }

    pub fn generateParser(self: *Self) ![]const u8 {
        try self.writeHeader();
        try self.writeASTNodeTypes();
        try self.writeParserStruct();
        try self.writeParserFunctions();
        try self.writeMainFunction();
        try self.writeTests();
        return self.output.toOwnedSlice();
    }

    fn writeHeader(self: *Self) !void {
        try self.output.appendSlice(self.allocator, "// Generated parser for ");
        try self.output.appendSlice(self.allocator, switch (self.grammar.format) {
            .ebnf => "EBNF",
            .pegz => "PEGZ",
        });
        try self.output.appendSlice(self.allocator, " grammar\n");
        try self.output.appendSlice(self.allocator, "// DO NOT EDIT - This file is generated automatically\n\n");

        try self.output.appendSlice(self.allocator, "const std = @import(\"std\");\n");
        try self.output.appendSlice(self.allocator, "const ast = @import(\"ast.zig\");\n\n");
    }

    fn writeASTNodeTypes(self: *Self) !void {
        try self.output.appendSlice("// Parse result types\n");
        try self.output.appendSlice("pub const ParseResult = struct {\n");
        try self.output.appendSlice("    success: bool,\n");
        try self.output.appendSlice("    node: ?ParseNode = null,\n");
        try self.output.appendSlice("    error_pos: usize = 0,\n");
        try self.output.appendSlice("    message: ?[]const u8 = null,\n");
        try self.output.appendSlice("};\n\n");

        try self.output.appendSlice("pub const ParseNode = struct {\n");
        try self.output.appendSlice("    rule_name: []const u8,\n");
        try self.output.appendSlice("    children: std.ArrayList(ParseNode),\n");
        try self.output.appendSlice("    value: ?[]const u8 = null,\n");
        try self.output.appendSlice("    start_pos: usize = 0,\n");
        try self.output.appendSlice("    end_pos: usize = 0,\n");
        try self.output.appendSlice("    allocator: std.mem.Allocator,\n\n");
        try self.output.appendSlice("    pub fn init(allocator: std.mem.Allocator, rule_name: []const u8) ParseNode {\n");
        try self.output.appendSlice("        return ParseNode{\n");
        try self.output.appendSlice("            .rule_name = rule_name,\n");
        try self.output.appendSlice("            .children = std.ArrayList(ParseNode).init(allocator),\n");
        try self.output.appendSlice("            .allocator = allocator,\n");
        try self.output.appendSlice("        };\n");
        try self.output.appendSlice("    }\n\n");
        try self.output.appendSlice("    pub fn deinit(self: *ParseNode) void {\n");
        try self.output.appendSlice("        for (self.children.items) |*child| {\n");
        try self.output.appendSlice("            child.deinit();\n");
        try self.output.appendSlice("        }\n");
        try self.output.appendSlice("        self.children.deinit();\n");
        try self.output.appendSlice("    }\n");
        try self.output.appendSlice("};\n\n");
    }

    fn writeParserStruct(self: *Self) !void {
        try self.output.appendSlice("pub const Parser = struct {\n");
        try self.output.appendSlice("    const Self = @This();\n");
        try self.output.appendSlice("    input: []const u8,\n");
        try self.output.appendSlice("    position: usize = 0,\n");
        try self.output.appendSlice("    allocator: std.mem.Allocator,\n\n");
        try self.output.appendSlice("    pub fn init(input: []const u8, allocator: std.mem.Allocator) Self {\n");
        try self.output.appendSlice("        return Self{\n");
        try self.output.appendSlice("            .input = input,\n");
        try self.output.appendSlice("            .allocator = allocator,\n");
        try self.output.appendSlice("        };\n");
        try self.output.appendSlice("    }\n\n");
        try self.output.appendSlice("    pub fn parse(self: *Self) !ParseResult {\n");
        if (self.grammar.rules.items.len > 0) {
            const start_rule = self.grammar.rules.items[0];
            try self.output.print("        return try self.parse_{s}();\n", .{start_rule.name});
        } else {
            try self.output.appendSlice("        return ParseResult{\n");
            try self.output.appendSlice("            .success = false,\n");
            try self.output.appendSlice("            .message = \"No rules defined\",\n");
            try self.output.appendSlice("        };\n");
        }
        try self.output.appendSlice("    }\n\n");

        // Write utility functions
        try self.writeParserUtilities();
    }

    fn writeParserUtilities(self: *Self) !void {
        try self.output.appendSlice("    fn isAtEnd(self: Self) bool {\n");
        try self.output.appendSlice("        return self.position >= self.input.len;\n");
        try self.output.appendSlice("    }\n\n");

        try self.output.appendSlice("    fn peek(self: Self) ?u8 {\n");
        try self.output.appendSlice("        if (self.isAtEnd()) return null;\n");
        try self.output.appendSlice("        return self.input[self.position];\n");
        try self.output.appendSlice("    }\n\n");

        try self.output.appendSlice("    fn advance(self: *Self) ?u8 {\n");
        try self.output.appendSlice("        if (self.isAtEnd()) return null;\n");
        try self.output.appendSlice("        self.position += 1;\n");
        try self.output.appendSlice("        return self.input[self.position - 1];\n");
        try self.output.appendSlice("    }\n\n");

        try self.output.appendSlice("    fn matchString(self: *Self, str: []const u8) bool {\n");
        try self.output.appendSlice("        if (self.position + str.len > self.input.len) {\n");
        try self.output.appendSlice("            return false;\n");
        try self.output.appendSlice("        }\n");
        try self.output.appendSlice("        const slice = self.input[self.position .. self.position + str.len];\n");
        try self.output.appendSlice("        if (std.mem.eql(u8, slice, str)) {\n");
        try self.output.appendSlice("            self.position += str.len;\n");
        try self.output.appendSlice("            return true;\n");
        try self.output.appendSlice("        }\n");
        try self.output.appendSlice("        return false;\n");
        try self.output.appendSlice("    }\n\n");

        try self.output.appendSlice("    fn matchCharClass(self: *Self, class_def: []const u8) bool {\n");
        try self.output.appendSlice("        if (self.isAtEnd()) return false;\n");
        try self.output.appendSlice("        const c = self.input[self.position];\n");
        try self.output.appendSlice("        for (class_def) |class_char| {\n");
        try self.output.appendSlice("            if (c == class_char) {\n");
        try self.output.appendSlice("                self.position += 1;\n");
        try self.output.appendSlice("                return true;\n");
        try self.output.appendSlice("            }\n");
        try self.output.appendSlice("        }\n");
        try self.output.appendSlice("        return false;\n");
        try self.output.appendSlice("    }\n\n");
    }

    fn writeParserFunctions(self: *Self) !void {
        for (self.grammar.rules.items) |rule| {
            try self.writeParserFunction(rule);
        }
    }

    fn writeParserFunction(self: *Self, rule: ast.Rule) !void {
        try self.output.print("    fn parse_{s}(self: *Self) !ParseResult {{\n", .{rule.name});

        // Save position for backtracking
        try self.output.appendSlice("        const start_pos = self.position;\n");
        try self.output.appendSlice("        var node = ParseNode.init(self.allocator, \"");
        try self.output.appendSlice(rule.name);
        try self.output.appendSlice("\");\n");
        try self.output.appendSlice("        node.start_pos = start_pos;\n\n");

        try self.writeExpressionParser(rule.expression, "        ");

        try self.output.appendSlice("        node.end_pos = self.position;\n");
        try self.output.appendSlice("        return ParseResult{\n");
        try self.output.appendSlice("            .success = true,\n");
        try self.output.appendSlice("            .node = node,\n");
        try self.output.appendSlice("        };\n");
        try self.output.appendSlice("    }\n\n");
    }

    fn writeExpressionParser(self: *Self, expr: ast.Expression, indent: []const u8) !void {
        switch (expr) {
            .choice => |choice| {
                try self.output.appendSlice(indent);
                try self.output.appendSlice("// Choice expression\n");
                for (choice.alternatives.items, 0..) |alt, i| {
                    if (i > 0) {
                        try self.output.appendSlice(indent);
                        try self.output.appendSlice("if (!result.success) {\n");
                        try self.output.appendSlice(indent);
                        try self.output.appendSlice("    self.position = start_pos;\n");
                    }
                    try self.writeActionExpressionParser(alt, indent ++ "    ");
                    if (i > 0) {
                        try self.output.appendSlice(indent);
                        try self.output.appendSlice("}\n");
                    }
                }
            },
            .action => |action| {
                try self.writeActionExpressionParser(action, indent);
            },
            .sequence => |sequence| {
                try self.writeSequenceExpressionParser(sequence, indent);
            },
            .labeled => |labeled| {
                try self.writeExpressionParser(labeled.expression, indent);
            },
            .prefixed => |prefixed| {
                try self.writePrefixedExpressionParser(prefixed, indent);
            },
            .suffixed => |suffixed| {
                try self.writeSuffixedExpressionParser(suffixed, indent);
            },
            .primary => |primary| {
                try self.writePrimaryExpressionParser(primary, indent);
            },
        }
    }

    fn writeActionExpressionParser(self: *Self, action: ast.ActionExpr, indent: []const u8) !void {
        try self.output.appendSlice(indent);
        try self.output.appendSlice("// Action expression\n");
        try self.writeExpressionParser(action.sequence, indent);
    }

    fn writeSequenceExpressionParser(self: *Self, sequence: ast.SeqExpr, indent: []const u8) !void {
        try self.output.appendSlice(indent);
        try self.output.appendSlice("// Sequence expression\n");
        for (sequence.expressions.items) |expr| {
            try self.writeExpressionParser(expr, indent);
        }
    }

    fn writePrefixedExpressionParser(self: *Self, prefixed: ast.PrefixedExpr, indent: []const u8) !void {
        if (prefixed.op) |op| {
            try self.output.appendSlice(indent);
            try self.output.appendSlice("// Prefixed expression: ");
            try self.output.appendSlice(switch (op) {
                .and_op => "&",
                .not_op => "!",
            });
            try self.output.appendSlice("\n");

            switch (op) {
                .and_op => {
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("const lookahead_pos = self.position;\n");
                    try self.writeExpressionParser(prefixed.expression, indent);
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("self.position = lookahead_pos;\n");
                },
                .not_op => {
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("const neg_start = self.position;\n");
                    try self.writeExpressionParser(prefixed.expression, indent);
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("if (result.success) {\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    self.position = neg_start;\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    return ParseResult{ .success = false };\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("}\n");
                },
            }
        } else {
            try self.writeExpressionParser(prefixed.expression, indent);
        }
    }

    fn writeSuffixedExpressionParser(self: *Self, suffixed: ast.SuffixedExpr, indent: []const u8) !void {
        try self.writeExpressionParser(suffixed.expression, indent);

        if (suffixed.op) |op| {
            try self.output.appendSlice(indent);
            try self.output.appendSlice("// Suffix operator: ");
            try self.output.appendSlice(switch (op) {
                .optional => "?",
                .zero_or_more => "*",
                .one_or_more => "+",
            });
            try self.output.appendSlice("\n");

            switch (op) {
                .optional => {
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("// Optional - already matched at least once, success anyway\n");
                },
                .zero_or_more => {
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("// Zero or more\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("while (true) {\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    const loop_pos = self.position;\n");
                    try self.writeExpressionParser(suffixed.expression, indent ++ "    ");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    if (!result.success) {\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("        self.position = loop_pos;\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("        break;\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    }\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("}\n");
                },
                .one_or_more => {
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("// One or more\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("while (true) {\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    const loop_pos = self.position;\n");
                    try self.writeExpressionParser(suffixed.expression, indent ++ "    ");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    if (!result.success) {\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("        self.position = loop_pos;\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("        break;\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("    }\n");
                    try self.output.appendSlice(indent);
                    try self.output.appendSlice("}\n");
                },
            }
        }
    }

    fn writePrimaryExpressionParser(self: *Self, primary: ast.PrimaryExpr, indent: []const u8) !void {
        try self.output.appendSlice(indent);
        try self.output.appendSlice("// Primary expression: ");
        switch (primary) {
            .literal => |literal| {
                try self.output.appendSlice("literal ");
                switch (literal.literal_type) {
                    .string => try self.output.appendSlice("string"),
                    .raw_string => try self.output.appendSlice("raw string"),
                    .character => try self.output.appendSlice("character"),
                }
                try self.output.appendSlice("\n");
                try self.output.appendSlice(indent);
                try self.output.print("if (!self.matchString(\"{s}\")) {{\n", .{std.zig.fmtEscapes(literal.value)});
                try self.output.appendSlice(indent);
                try self.output.appendSlice("    return ParseResult{ .success = false };\n");
                try self.output.appendSlice(indent);
                try self.output.appendSlice("}\n");
            },
            .char_class => |char_class| {
                try self.output.appendSlice("character class\n");
                try self.output.appendSlice(indent);
                try self.output.print("if (!self.matchCharClass(\"{s}\")) {{\n", .{std.zig.fmtEscapes(char_class.class_definition)});
                try self.output.appendSlice(indent);
                try self.output.appendSlice("    return ParseResult{ .success = false };\n");
                try self.output.appendSlice(indent);
                try self.output.appendSlice("}\n");
            },
            .any_matcher => {
                try self.output.appendSlice("any character\n");
                try self.output.appendSlice(indent);
                try self.output.appendSlice("if (self.advance() == null) {\n");
                try self.output.appendSlice(indent);
                try self.output.appendSlice("    return ParseResult{ .success = false };\n");
                try self.output.appendSlice(indent);
                try self.output.appendSlice("}\n");
            },
            .rule_ref => |rule_ref| {
                try self.output.appendSlice("rule reference ");
                try self.output.appendSlice(rule_ref.rule_name);
                try self.output.appendSlice("\n");
                try self.output.appendSlice(indent);
                try self.output.print("result = try self.parse_{s}();\n", .{rule_ref.rule_name});
                try self.output.appendSlice(indent);
                try self.output.appendSlice("if (!result.success) return result;\n");
            },
            .grouped => |grouped| {
                try self.output.appendSlice("grouped expression\n");
                try self.writeExpressionParser(grouped.expression, indent);
            },
        }
    }

    fn writeMainFunction(self: *Self) !void {
        try self.output.appendSlice("pub fn parse(input: []const u8, allocator: std.mem.Allocator) !ParseResult {\n");
        try self.output.appendSlice("    var parser = Parser.init(input, allocator);\n");
        try self.output.appendSlice("    return parser.parse();\n");
        try self.output.appendSlice("}\n\n");
    }

    fn writeTests(self: *Self) !void {
        try self.output.appendSlice("// Test functions\n");
        try self.output.appendSlice("test \"basic parsing\" {\n");
        try self.output.appendSlice("    const allocator = std.testing.allocator;\n");
        try self.output.appendSlice("    const input = \"\";\n"); // TODO: Add sample input
        try self.output.appendSlice("    const result = try parse(input, allocator);\n");
        try self.output.appendSlice("    defer if (result.node) |node| node.deinit();\n");
        try self.output.appendSlice("    try std.testing.expect(result.success);\n");
        try self.output.appendSlice("}\n");
    }
};