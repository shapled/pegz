const std = @import("std");
const prepegz = @import("parser.zig");
const builder_mod = @import("pegz_common").builder;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .thread_safe = false,
    }).init;
    defer {
        _ = gpa.deinit();
    }

    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer allocator.free(args);

    if (args.len < 2) {
        std.log.err("USAGE: prepegz [-o OUTPUT] GRAMMAR_FILE\n", .{});
        std.log.err("Example: prepegz grammars/pegz.pegz -o pegz/parser.zig\n", .{});
        std.process.exit(1);
    }

    var output_file: ?[]const u8 = null;
    var grammar_file: []const u8 = "";

    // Parse command line arguments
    var i: usize = 1;
    while (i < args.len) {
        if (std.mem.eql(u8, args[i], "-o")) {
            if (i + 1 >= args.len) {
                std.log.err("Error: -o flag requires an output filename\n", .{});
                std.process.exit(2);
            }
            output_file = args[i + 1];
            i += 2;
        } else if (grammar_file.len == 0) {
            grammar_file = args[i];
            i += 1;
        } else {
            std.log.err("Error: Unexpected argument: {s}\n", .{args[i]});
            std.process.exit(2);
        }
    }

    if (grammar_file.len == 0) {
        std.log.err("Error: Grammar file path required\n", .{});
        std.process.exit(1);
    }

    // Set default output if none provided
    const final_output = if (output_file) |path| path else "parser.zig";

    // Create output writer
    var buffer: [4096]u8 = undefined;
    const output_dir = std.fs.path.dirname(final_output) orelse ".";

    std.fs.cwd().makePath(output_dir) catch |err| {
        std.log.err("Error creating output directory '{s}': {}\n", .{ output_dir, err });
        std.process.exit(3);
    };

    const out_file = std.fs.cwd().createFile(final_output, .{ .truncate = true, .mode = 0o644 }) catch |err| {
        std.log.err("Error creating output file '{s}': {}\n", .{ final_output, err });
        std.process.exit(4);
    };
    defer out_file.close();

    // Create writer using the same pattern as bootstrap
    var file_writer = out_file.writer(&buffer);
    const file_writer_interface = &file_writer.interface;

    // Open and read grammar file
    const in_file = std.fs.cwd().openFile(grammar_file, .{ .mode = .read_only }) catch |err| {
        std.log.err("Error opening grammar file '{s}': {}\n", .{ grammar_file, err });
        std.process.exit(5);
    };
    defer in_file.close();

    var file_reader = in_file.reader(&buffer);

    // Initialize parser and parse grammar
    const grammar = prepegz.parse(allocator, grammar_file, &file_reader.interface) catch |err| {
        std.log.err("Error parsing grammar file: {}\n", .{err});
        std.process.exit(6);
    };

    // Create builder and generate parser code
    var builder = try builder_mod.Builder.init(allocator, file_writer_interface, .{});
    defer builder.deinit();

    try builder.buildParser(grammar);

    // Flush the writer
    try file_writer_interface.flush();

    // Success message
    std.debug.print("✅ Successfully generated parser: {s}\n", .{final_output});
    std.debug.print("   from grammar: {s}\n", .{grammar_file});
    std.debug.print("   using prepegz version\n", .{});
}
