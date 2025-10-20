const std = @import("std");
const ast = @import("ast.zig");

// Simplified code generator for bootstrap phase
pub const CodeGenerator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    grammar: *ast.Grammar,

    pub fn init(allocator: std.mem.Allocator, grammar: *ast.Grammar) Self {
        return Self{
            .allocator = allocator,
            .grammar = grammar,
        };
    }

    pub fn deinit(self: *Self) void {
        self.* = undefined;
    }

    pub fn generateParser(self: *Self) ![]const u8 {
        var buffer = std.ArrayList(u8).initCapacity(self.allocator, 4096) catch return error.OutOfMemory;

        try buffer.appendSlice(self.allocator, "// Generated parser for ");
        const format_name = switch (self.grammar.format) {
            .ebnf => "EBNF",
            .pegz => "PEGZ",
        };
        try buffer.appendSlice(self.allocator, format_name);
        try buffer.appendSlice(self.allocator, " grammar\n");
        try buffer.appendSlice(self.allocator, "// DO NOT EDIT - This file is generated automatically\n\n");

        try buffer.appendSlice(self.allocator, "const std = @import(\"std\");\n\n");

        try buffer.appendSlice(self.allocator, "pub const ParseResult = struct {\n");
        try buffer.appendSlice(self.allocator, "    success: bool,\n");
        try buffer.appendSlice(self.allocator, "    message: ?[]const u8 = null,\n");
        try buffer.appendSlice(self.allocator, "};\n\n");

        try buffer.appendSlice(self.allocator, "pub const Parser = struct {\n");
        try buffer.appendSlice(self.allocator, "    input: []const u8,\n");
        try buffer.appendSlice(self.allocator, "    position: usize = 0,\n\n");

        try buffer.appendSlice(self.allocator, "    pub fn init(input: []const u8) Parser {\n");
        try buffer.appendSlice(self.allocator, "        return Parser{ .input = input };\n");
        try buffer.appendSlice(self.allocator, "    }\n\n");

        // Generate parser functions for each rule
        for (self.grammar.rules.items) |rule| {
            try buffer.appendSlice(self.allocator, "    pub fn parse_");
            try buffer.appendSlice(self.allocator, rule.name);
            try buffer.appendSlice(self.allocator, "(self: *Parser) ParseResult {\n");
            try buffer.appendSlice(self.allocator, "        // TODO: Implement parsing for rule ");
            try buffer.appendSlice(self.allocator, rule.name);
            try buffer.appendSlice(self.allocator, "\n        _ = self;\n");
            try buffer.appendSlice(self.allocator, "        return ParseResult{ .success = true };\n");
            try buffer.appendSlice(self.allocator, "    }\n\n");
        }

        // Generate main parse function
        if (self.grammar.rules.items.len > 0) {
            const start_rule = self.grammar.rules.items[0];
            try buffer.appendSlice(self.allocator, "    pub fn parse(self: *Parser) ParseResult {\n");
            try buffer.appendSlice(self.allocator, "        return self.parse_");
            try buffer.appendSlice(self.allocator, start_rule.name);
            try buffer.appendSlice(self.allocator, "();\n");
            try buffer.appendSlice(self.allocator, "    }\n");
        }

        try buffer.appendSlice(self.allocator, "};\n\n");

        try buffer.appendSlice(self.allocator, "pub fn parse(input: []const u8) ParseResult {\n");
        try buffer.appendSlice(self.allocator, "    var parser = Parser.init(input);\n");
        try buffer.appendSlice(self.allocator, "    return parser.parse();\n");
        try buffer.appendSlice(self.allocator, "}\n\n");

        try buffer.appendSlice(self.allocator, "test \"basic parsing\" {\n");
        try buffer.appendSlice(self.allocator, "    const result = parse(\"\");\n");
        try buffer.appendSlice(self.allocator, "    // TODO: Add actual tests\n");
        try buffer.appendSlice(self.allocator, "}\n");

        return buffer.toOwnedSlice(self.allocator);
    }
};