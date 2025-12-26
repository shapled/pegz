//! Unit tests for the Scanner (tokenizer)
//! Task 4.3: Scanner 词法分析测试

const std = @import("std");
const bootstrap = @import("bootstrap");
const Scanner = bootstrap.scan.Scanner;
const Tid = bootstrap.token.Tid;

const testing = std.testing;

test "Scanner - tokenize identifier" {
    // Use arena allocator for automatic memory management
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    // Create temporary file with test data
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("Identifier");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.ident, tok.id);
    try testing.expectEqualStrings("Identifier", tok.lit);
    // arena.deinit() will clean up tok.lit automatically
}

test "Scanner - tokenize string literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("\"hello\"");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.str, tok.id);
    try testing.expectEqualStrings("\"hello\"", tok.lit);
}

test "Scanner - tokenize char literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("'a'");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.char, tok.id);
    try testing.expectEqualStrings("'a'", tok.lit);
}

test "Scanner - tokenize character class" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("[a-z]");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.class, tok.id);
    try testing.expectEqualStrings("[a-z]", tok.lit);
}

test "Scanner - tokenize rule definition" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("<-");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.ruledef, tok.id);
}

test "Scanner - tokenize multiple tokens" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("Rule <- 'a'");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok1 = try scanner.scan();
    try testing.expectEqual(Tid.ident, tok1.id);
    try testing.expectEqualStrings("Rule", tok1.lit);

    const tok2 = try scanner.scan();
    try testing.expectEqual(Tid.ruledef, tok2.id);

    const tok3 = try scanner.scan();
    try testing.expectEqual(Tid.char, tok3.id);
    try testing.expectEqualStrings("'a'", tok3.lit);
}

test "Scanner - tokenize dot matcher" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll(".");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.dot, tok.id);
}

test "Scanner - tokenize left arrow (unicode)" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("←");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.ruledef, tok.id);
}

test "Scanner - tokenize with whitespace" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("  Rule  <-  'a'  ");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok1 = try scanner.scan();
    try testing.expectEqual(Tid.ident, tok1.id);
    try testing.expectEqualStrings("Rule", tok1.lit);

    const tok2 = try scanner.scan();
    try testing.expectEqual(Tid.ruledef, tok2.id);

    const tok3 = try scanner.scan();
    try testing.expectEqual(Tid.char, tok3.id);
    try testing.expectEqualStrings("'a'", tok3.lit);
}

test "Scanner - tokenize string with escape sequences" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("\"hello\\nworld\\t\"");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.str, tok.id);
    try testing.expectEqualStrings("\"hello\\nworld\\t\"", tok.lit);
}

test "Scanner - tokenize hex char literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    
    const file = try tmp.dir.createFile("test_input", .{.read = true});
    try file.writeAll("'\\x41'");
    try file.seekTo(0);
    defer file.close();
    
    var buffer: [1024]u8 = undefined;
    var file_reader = file.reader(&buffer);
    
    var scanner = try Scanner.init(allocator, "test.pegz", &file_reader.interface, null);
    defer scanner.deinit();

    const tok = try scanner.scan();
    try testing.expectEqual(Tid.char, tok.id);
    try testing.expectEqualStrings("'\\x41'", tok.lit);
}
