//! Unit tests for Builder (code generator)
//! Task 4.3: Builder 测试

const std = @import("std");
const ast = @import("pegz_common").ast;
const builder_mod = @import("pegz_common").builder;
const Builder = builder_mod.Builder;
const Options = builder_mod.Options;
const BuilderError = builder_mod.BuilderError;

const testing = std.testing;

test "Builder - init with default options" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_output", .{ .read = true });
    defer file.close();

    var buffer = [_]u8{0} ** 8192;
    var file_writer = file.writer(&buffer);
    const writer_interface = &file_writer.interface;

    const builder = try Builder.init(allocator, writer_interface, .{});

    try testing.expectEqual(@as(usize, 0), builder.rule_index);
    try testing.expectEqual(@as(usize, 0), builder.expr_index);
}

test "Builder - init with custom options" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_output", .{ .read = true });
    defer file.close();

    var buffer = [_]u8{0} ** 8192;
    var file_writer = file.writer(&buffer);
    const writer_interface = &file_writer.interface;

    const options = Options{
        .receiver_name = "parser",
        .optimize = true,
        .support_left_recursion = true,
    };

    const builder = try Builder.init(allocator, writer_interface, options);

    try testing.expectEqualStrings("parser", builder.options.receiver_name);
    try testing.expectEqual(true, builder.options.optimize);
    try testing.expectEqual(true, builder.options.support_left_recursion);
}

test "Builder - buildParser generates code" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_output", .{ .read = true });
    defer file.close();

    var buffer = [_]u8{0} ** 8192;
    var file_writer = file.writer(&buffer);
    const writer_interface = &file_writer.interface;

    // Create a simple grammar with one rule
    const pos = ast.Pos.init(1, 1, 0);
    const name_ident = ast.Identifier{ .pos = pos, .value = "TestRule" };
    const display_name = ast.StringLit{ .pos = pos, .value = "TestRule" };

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    const rule = try ast.Rule.create(allocator, pos, name_ident, display_name, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Build parser - if it doesn't crash, test passes
    var builder = try Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();
}

test "Builder - generates rule definitions" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_output", .{ .read = true });
    defer file.close();

    var buffer = [_]u8{0} ** 8192;
    var file_writer = file.writer(&buffer);
    const writer_interface = &file_writer.interface;

    // Create grammar with multiple rules
    const pos = ast.Pos.init(1, 1, 0);

    // Rule 1: First <- 'first'
    const name1 = ast.Identifier{ .pos = pos, .value = "First" };
    const display1 = ast.StringLit{ .pos = pos, .value = "First" };
    const lit1 = try ast.LitMatcher.create(allocator, pos, "first", false);
    const expr1 = ast.Expression{ .lit_matcher = lit1 };
    const rule1 = try ast.Rule.create(allocator, pos, name1, display1, expr1);

    // Rule 2: Second <- 'second'
    const name2 = ast.Identifier{ .pos = pos, .value = "Second" };
    const display2 = ast.StringLit{ .pos = pos, .value = "Second" };
    const lit2 = try ast.LitMatcher.create(allocator, pos, "second", false);
    const expr2 = ast.Expression{ .lit_matcher = lit2 };
    const rule2 = try ast.Rule.create(allocator, pos, name2, display2, expr2);

    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{ rule1, rule2 });

    // Build parser - if it doesn't crash, test passes
    var builder = try Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();
}
