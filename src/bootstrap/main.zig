const std = @import("std");
const ast = @import("../common/ast.zig");
const codegen = @import("../common/codegen.zig");
const ebnf_parser = @import("ebnf_parser.zig");

// Bootstrap main entry point
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <ebnf_file> [output_file]\n", .{args[0]});
        std.debug.print("  ebnf_file: Path to EBNF grammar file\n");
        std.debug.print("  output_file: Optional output file for generated parser (default: parser.zig)\n");
        return;
    }

    const input_file = args[1];
    const output_file = if (args.len > 2) args[2] else "parser.zig";

    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024 * 10); // 10MB limit
    defer allocator.free(input);

    // Parse EBNF grammar
    std.debug.print("Parsing EBNF grammar from {s}...\n", .{input_file});
    var grammar = try ebnf_parser.parseEbnfGrammar(allocator, input);
    defer grammar.deinit();

    std.debug.print("Successfully parsed {d} rules:\n", .{grammar.rules.items.len});
    for (grammar.rules.items) |rule| {
        std.debug.print("  - {s}\n", .{rule.name});
        if (rule.display_name) |display| {
            std.debug.print("    Display name: {s}\n", .{display});
        }
    }

    // Generate parser code
    std.debug.print("\nGenerating parser code...\n");
    var generator = codegen.CodeGenerator.init(allocator, &grammar);
    defer generator.deinit();

    const parser_code = try generator.generateParser();
    defer allocator.free(parser_code);

    // Write output file
    std.debug.print("Writing generated parser to {s}...\n", .{output_file});
    try std.fs.cwd().writeFile(.{ .sub_path = output_file, .data = parser_code });

    std.debug.print("Bootstrap completed successfully!\n");
    std.debug.print("Generated parser: {s} ({d} bytes)\n", .{ output_file, parser_code.len });
}

// Test function to validate bootstrap process
test "bootstrap test" {
    const allocator = std.testing.allocator;

    const test_grammar =
        \\Grammar = RuleList .
        \\RuleList = Rule { Rule } .
        \\Rule = ident ruledef Expression semicolon .
        \\Expression = ChoiceExpr .
        \\ChoiceExpr = ActionExpr { "/" ActionExpr } .
        \\ActionExpr = SeqExpr .
        \\SeqExpr = LabeledExpr { LabeledExpr } .
        \\LabeledExpr = PrefixedExpr .
        \\PrefixedExpr = [ ampersand ] SuffixedExpr .
        \\SuffixedExpr = PrimaryExpr [ star ] .
        \\PrimaryExpr = LiteralMatcher | RuleRefExpr .
        \\LiteralMatcher = str .
        \\RuleRefExpr = ident .
    ;

    var grammar = try ebnf_parser.parseEbnfGrammar(allocator, test_grammar);
    defer grammar.deinit();

    try std.testing.expect(grammar.rules.items.len > 0);
    try std.testing.expectEqualStrings("Grammar", grammar.rules.items[0].name);

    var generator = codegen.CodeGenerator.init(allocator, &grammar);
    defer generator.deinit();

    const parser_code = try generator.generateParser();
    defer allocator.free(parser_code);

    try std.testing.expect(parser_code.len > 0);
    try std.testing.expect(std.mem.indexOf(u8, parser_code, "pub const Parser") != null);
}