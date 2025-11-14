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

    var out_file = if (output_file) |outfile_path|
        std.fs.createFileAbsolute(outfile_path, .{}) catch |err| {
            std.log.err("Error creating output file: {}", .{err});
            std.process.exit(2);
        }
    else
        null;

    var out_writer: std.fs.File.Writer = undefined;
    var should_close_out = false;
    var buffer: [4096]u8 = undefined;

    if (out_file) |*file| {
        out_writer = file.writer(&buffer);
        should_close_out = true;
    } else {
        out_writer = std.fs.File.stdout().writer(&buffer);
    }

    defer {
        if (should_close_out) {
            if (out_file) |*file| {
                file.close();
            }
        }
    }

    // Parse input file
    const in_file = std.fs.cwd().openFile(input_file, .{.mode = .read_only}) catch |err| {
        std.log.err("Error opening input file: {}", .{err});
        std.process.exit(2);
    };
    defer in_file.close();

    var parser = bootstrap.Parser.init(allocator);
    defer parser.deinit();

    var file_reader = in_file.reader(&buffer);
    const grammar = parser.parse(input_file, &file_reader.interface) catch |err| {
        std.log.err("Error parsing input file: {}", .{err});
        std.process.exit(5);
    };

    // Generate parser code
    var output_buffer: std.ArrayList(u8) = .empty;
    defer output_buffer.deinit(allocator);

    const writer = std.io.Writer.Allocating.fromArrayList(allocator, &output_buffer);
    var builder = try builder_mod.Builder.init(allocator, writer.writer, .{});
    defer builder.deinit();

    builder.buildParser(grammar) catch |err| {
        std.log.err("Build error: {}", .{err});
        std.process.exit(6);
    };

    // Write output
    const generated_code = output_buffer.items;
    out_writer.interface.writeAll(generated_code) catch |err| {
        std.log.err("Error writing output: {}", .{err});
        std.process.exit(7);
    };
}
