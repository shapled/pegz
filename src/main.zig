const std = @import("std");
const pegz = @import("pegz");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try printUsage(args[0]);
        return;
    }

    if (std.mem.eql(u8, args[1], "bootstrap")) {
        // Run bootstrap phase
        if (args.len < 3) {
            std.debug.print("Usage: {s} bootstrap <ebnf_file> [output_file]\n", .{args[0]});
            return;
        }

        const input_file = args[2];
        const output_file = if (args.len > 3) args[3] else "parser.zig";

        try runBootstrap(allocator, input_file, output_file);
    } else {
        // Default to bootstrap for backward compatibility
        try printUsage(args[0]);
    }
}

fn printUsage(program_name: []const u8) !void {
    std.debug.print("Pegz Parser Generator\n\n", .{});
    std.debug.print("Usage: {s} <command> [options]\n", .{program_name});
    std.debug.print("\nCommands:\n", .{});
    std.debug.print("  bootstrap <ebnf_file> [output_file]  Generate parser from EBNF grammar\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  {s} bootstrap pegz.ebnf\n", .{program_name});
    std.debug.print("  {s} bootstrap pegz.ebnf custom_parser.zig\n", .{program_name});
}

fn runBootstrap(allocator: std.mem.Allocator, input_file: []const u8, output_file: []const u8) !void {
    // Read input file
    const input = try std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024 * 10); // 10MB limit
    defer allocator.free(input);

    // Parse EBNF grammar
    std.debug.print("Parsing EBNF grammar from {s}...\n", .{input_file});
    var grammar = try pegz.bootstrap.ebnf_parser.parseEbnfGrammar(allocator, input);
    defer grammar.deinit();

    std.debug.print("Successfully parsed {d} rules:\n", .{grammar.rules.items.len});
    for (grammar.rules.items) |rule| {
        std.debug.print("  - {s}\n", .{rule.name});
        if (rule.display_name) |display| {
            std.debug.print("    Display name: {s}\n", .{display});
        }
    }

    // Generate parser code
    std.debug.print("\nGenerating parser code...\n", .{});
    var generator = pegz.codegen.CodeGenerator.init(allocator, &grammar);
    defer generator.deinit();

    const parser_code = try generator.generateParser();
    defer allocator.free(parser_code);

    // Write output file
    std.debug.print("Writing generated parser to {s}...\n", .{output_file});
    try std.fs.cwd().writeFile(.{ .sub_path = output_file, .data = parser_code });

    std.debug.print("Bootstrap completed successfully!\n", .{});
    std.debug.print("Generated parser: {s} ({d} bytes)\n", .{ output_file, parser_code.len });
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list: std.ArrayList(i32) = .empty;
    defer list.deinit(gpa); // Try commenting this out and see if zig detects the memory leak!
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}
