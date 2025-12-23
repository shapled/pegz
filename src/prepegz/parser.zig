const std = @import("std");
const ast = @import("pegz_common").ast;

// Reuse bootstrap parser since it already implements full PEG grammar parsing
const bootstrap_parser = @import("bootstrap");
pub const Parser = bootstrap_parser.Parser;

/// Public API - matches bootstrap parser interface
pub fn parse(allocator: std.mem.Allocator, filename: []const u8, reader: *std.io.Reader) !*ast.Grammar {
    var parser = Parser.init(allocator);
    return try parser.parse(filename, reader);
}
