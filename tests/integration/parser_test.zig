//! Integration tests for complete parsing workflow
//! Task 4.4: Parser 集成测试

const std = @import("std");
const ast = @import("pegz_common").ast;
const builder_mod = @import("pegz_common").builder;

const testing = std.testing;

test "Integration - builder can generate parser from simple grammar AST" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a simple test grammar manually
    const pos = ast.Pos.init(1, 1, 0);

    const name = ast.Identifier{ .pos = pos, .value = "Hello" };
    const display = ast.StringLit{ .pos = pos, .value = "Hello" };

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "hello", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Generate parser code
    const output_path = ".zig-cache/integration_test_parser.zig";

    // Ensure .zig-cache directory exists
    try std.fs.cwd().makePath(".zig-cache");

    const out_file = try std.fs.cwd().createFile(output_path, .{ .read = true, .truncate = true });
    defer out_file.close();

    var write_buffer: [1024 * 1024]u8 = undefined;
    var file_writer = out_file.writer(&write_buffer);
    const writer_interface = &file_writer.interface;

    var builder = try builder_mod.Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();

    // Check that output file was created
    const stat = try std.fs.cwd().statFile(output_path);
    try testing.expect(stat.size > 0);
}

test "Integration - builder can generate parser from complex grammar AST" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Create a more complex test grammar with multiple rules
    const pos = ast.Pos.init(1, 1, 0);

    // Rule 1: Expr <- Term (('+' / '-') Term)*
    const name1 = ast.Identifier{ .pos = pos, .value = "Expr" };
    const display1 = ast.StringLit{ .pos = pos, .value = "Expr" };

    const lit_plus = try ast.LitMatcher.create(allocator, pos, "+", false);
    const expr1 = ast.Expression{ .lit_matcher = lit_plus };

    const lit_minus = try ast.LitMatcher.create(allocator, pos, "-", false);
    const expr2 = ast.Expression{ .lit_matcher = lit_minus };

    const choice = try ast.ChoiceExpr.create(allocator, pos, &[_]ast.Expression{ expr1, expr2 });
    const expr3 = ast.Expression{ .choice = choice };

    const name_term = ast.Identifier{ .pos = pos, .value = "Term" };
    const ref_term = try ast.RuleRefExpr.create(allocator, pos, name_term);
    const expr4 = ast.Expression{ .rule_ref = ref_term };

    const zero_or_more_expr = try ast.ZeroOrMoreExpr.create(allocator, pos, expr4);
    const zero_or_more = ast.Expression{ .zero_or_more = zero_or_more_expr };
    const seq = try ast.SeqExpr.create(allocator, pos, &[_]ast.Expression{ expr3, zero_or_more });
    const expr_final = ast.Expression{ .seq = seq };

    const rule1 = try ast.Rule.create(allocator, pos, name1, display1, expr_final);

    // Rule 2: Term <- Number
    const name2 = ast.Identifier{ .pos = pos, .value = "Term" };
    const display2 = ast.StringLit{ .pos = pos, .value = "Term" };

    const name_num = ast.Identifier{ .pos = pos, .value = "Number" };
    const ref_num = try ast.RuleRefExpr.create(allocator, pos, name_num);
    const expr_num = ast.Expression{ .rule_ref = ref_num };

    const rule2 = try ast.Rule.create(allocator, pos, name2, display2, expr_num);

    // Rule 3: Number <- [0-9]+
    const name3 = ast.Identifier{ .pos = pos, .value = "Number" };
    const display3 = ast.StringLit{ .pos = pos, .value = "Number" };

    const char_class = try ast.CharClassMatcher.create(
        allocator,
        pos,
        "[0-9]",
        false,
        false,
        &[_]u8{},
        &[_]struct { u8, u8 }{.{ '0', '9' }},
        &[_][]const u8{},
    );
    const expr_cc = ast.Expression{ .char_class_matcher = char_class };

    const one_or_more_expr = try ast.OneOrMoreExpr.create(allocator, pos, expr_cc);
    const expr_final3 = ast.Expression{ .one_or_more = one_or_more_expr };

    const rule3 = try ast.Rule.create(allocator, pos, name3, display3, expr_final3);

    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{ rule1, rule2, rule3 });

    // Generate parser code
    const output_path = ".zig-cache/integration_complex_parser.zig";

    const out_file = try std.fs.cwd().createFile(output_path, .{ .read = true, .truncate = true });
    defer out_file.close();

    var write_buffer: [1024 * 1024]u8 = undefined;
    var file_writer = out_file.writer(&write_buffer);
    const writer_interface = &file_writer.interface;

    var builder = try builder_mod.Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();

    // Check that output file was created and is substantial
    const stat = try std.fs.cwd().statFile(output_path);
    try testing.expect(stat.size > 1000); // Complex grammar should generate substantial code
}

test "Integration - builder handles empty grammar" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);
    const grammar = try ast.Grammar.create(allocator, pos, null);

    // Generate parser code
    const output_path = ".zig-cache/integration_empty_parser.zig";

    const out_file = try std.fs.cwd().createFile(output_path, .{ .read = true, .truncate = true });
    defer out_file.close();

    var write_buffer: [1024 * 1024]u8 = undefined;
    var file_writer = out_file.writer(&write_buffer);
    const writer_interface = &file_writer.interface;

    var builder = try builder_mod.Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();

    // Check that output file was created
    const stat = try std.fs.cwd().statFile(output_path);
    try testing.expect(stat.size > 0);
}

test "Integration - builder handles grammar with init code" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const pos = ast.Pos.init(1, 1, 0);

    // Create an init code block with actual Zig code
    const init_code = try allocator.create(ast.CodeBlock);
    init_code.* = try ast.CodeBlock.init(allocator, pos, "const std = @import(\"std\");\nvar global_counter: u32 = 0;");

    const name = ast.Identifier{ .pos = pos, .value = "Test" };
    const display = ast.StringLit{ .pos = pos, .value = "Test" };

    const lit_matcher = try ast.LitMatcher.create(allocator, pos, "test", false);
    const expr = ast.Expression{ .lit_matcher = lit_matcher };

    const rule = try ast.Rule.create(allocator, pos, name, display, expr);
    const grammar = try ast.Grammar.create(allocator, pos, &[_]*ast.Rule{rule});

    // Set the init code
    grammar.init = init_code;

    // Generate parser code
    const output_path = ".zig-cache/integration_init_parser.zig";

    const out_file = try std.fs.cwd().createFile(output_path, .{ .read = true, .truncate = true });
    defer out_file.close();

    var write_buffer: [1024 * 1024]u8 = undefined;
    var file_writer = out_file.writer(&write_buffer);
    const writer_interface = &file_writer.interface;

    var builder = try builder_mod.Builder.init(allocator, writer_interface, .{});
    try builder.buildParser(grammar);
    try writer_interface.flush();

    // Read generated file and verify init code is present
    const content = try out_file.getEndPos();
    try out_file.seekTo(0);
    const file_content = try allocator.alloc(u8, content);
    _ = try out_file.readAll(file_content);

    // Check that init code is in the generated file
    const content_str = std.mem.sliceTo(file_content, 0);

    // Verify the init code section exists
    try testing.expect(std.mem.indexOf(u8, content_str, "// Init code") != null);
    try testing.expect(std.mem.indexOf(u8, content_str, "const std = @import(\"std\");") != null);
    try testing.expect(std.mem.indexOf(u8, content_str, "var global_counter: u32 = 0;") != null);
}
