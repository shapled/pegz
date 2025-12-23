const std = @import("std");
const bootstrap = @import("parser.zig");
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
        std.log.err("USAGE: bootstrap-build [-o OUTPUT] FILE\n", .{});
        std.process.exit(1);
    }

    var output_file: ?[]const u8 = null;
    var input_file: []const u8 = "";

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
        } else if (input_file.len == 0) {
            input_file = args[i];
            i += 1;
        } else {
            std.log.err("Error: Unexpected argument: {s}", .{args[i]});
            std.process.exit(2);
        }
    }

    if (input_file.len == 0) {
        std.log.err("Error: Input file required\n", .{});
        std.process.exit(1);
    }

    // Parse input file
    var buffer: [4096]u8 = undefined;
    const in_file = std.fs.cwd().openFile(input_file, .{.mode = .read_only}) catch |err| {
        std.log.err("Error opening input file: {}", .{err});
        std.process.exit(5);
    };
    defer in_file.close();

    var parser = bootstrap.Parser.init(allocator);
    defer parser.deinit();

    var file_reader = in_file.reader(&buffer);
    const grammar = parser.parse(input_file, &file_reader.interface) catch |err| {
        std.log.err("Error parsing input file: {}", .{err});
        std.process.exit(5);
    };

    // Create writer based on output
    if (output_file) |outfile_path| {
        // Check if path is absolute or relative
        const file = if (std.fs.path.isAbsolute(outfile_path))
            try std.fs.createFileAbsolute(outfile_path, .{})
        else
            try std.fs.cwd().createFile(outfile_path, .{});
        defer file.close();

        // Create writer
        var file_writer = file.writer(&buffer);
        const file_writer_interface = &file_writer.interface;

        // Create builder
        var builder = try builder_mod.Builder.init(allocator, file_writer_interface, .{});
        defer builder.deinit();

        builder.buildParser(grammar) catch |err| {
            std.log.err("Build error: {}", .{err});
            std.process.exit(6);
        };

        try file_writer_interface.flush();
    } else {
        // Use stdout
        var stdout_buffer: [4096]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
        const stdout = &stdout_writer.interface;

        var builder = try builder_mod.Builder.init(allocator, stdout, .{});
        defer builder.deinit();

        builder.buildParser(grammar) catch |err| {
            std.log.err("Build error: {}", .{err});
            std.process.exit(6);
        };

        try stdout.flush();
    }
}
