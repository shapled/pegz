//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

// Common modules (shared between bootstrap and self-hosting phases)
pub const ast = @import("common/ast.zig");
pub const lexer = @import("common/lexer.zig");
pub const parser = @import("common/parser.zig");
pub const codegen = @import("common/codegen_simple.zig");

// Bootstrap phase modules
pub const bootstrap = struct {
    pub const ebnf_lexer = @import("bootstrap/ebnf_lexer.zig");
    pub const ebnf_parser = @import("bootstrap/ebnf_parser.zig");
    pub const main = @import("bootstrap/main.zig");
};

pub fn bufferedPrint() !void {
    // Stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.flush(); // Don't forget to flush!
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try std.testing.expect(add(3, 7) == 10);
}
