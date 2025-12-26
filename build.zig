const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const common_module = b.createModule(.{
        .root_source_file = b.path("src/common/mod.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });

    // -------------------- Stage 1: Bootstrap --------------------
    const bootstrap_exe = b.addExecutable(.{
        .name = "bootstrap",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/bootstrap/main.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    bootstrap_exe.root_module.addImport("pegz_common", common_module);
    b.installArtifact(bootstrap_exe);

    // Generate src/prepegz/parser.zig from grammars/prepegz.pegz
    const generate_prepegz_parser = b.addRunArtifact(bootstrap_exe);
    generate_prepegz_parser.addArgs(&[_][]const u8{
        "./grammars/prepegz.pegz",
        "-o",
        "src/prepegz/parser.zig",
    });

    // -------------------- Stage 2: Prepegz --------------------
    // Compile the prepegz executable using generated parser
    const prepegz_exe = b.addExecutable(.{
        .name = "prepegz",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/prepegz/main.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    prepegz_exe.root_module.addImport("pegz_common", common_module);
    // Add bootstrap as a dependency so prepegz can reuse its parser
    const bootstrap_mod = b.createModule(.{
        .root_source_file = b.path("src/bootstrap/mod.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    bootstrap_mod.addImport("pegz_common", common_module);
    prepegz_exe.root_module.addImport("bootstrap", bootstrap_mod);


    // Ensure prepegz/grammar.zig is generated before compiling prepegz_exe
    prepegz_exe.step.dependOn(&generate_prepegz_parser.step);
    b.installArtifact(prepegz_exe);

    // -------------------- Stage 3: Generate pegz/parser.zig --------------------
    // Use the full pegz.pegz grammar with advanced features
    const generate_pegz_parser = b.addRunArtifact(prepegz_exe);

    generate_pegz_parser.addArgs(&[_][]const u8{
        "./grammars/pegz.pegz",  // Full-featured pegz grammar
        "-o",
        "src/pegz/parser.zig",
    });

    // Ensure prepegz_exe is built before generating pegz/parser.zig
    generate_pegz_parser.step.dependOn(&prepegz_exe.step);

    // -------------------- Stage 4: Pegz --------------------
    // Compile the pegz executable using generated parser
    const pegz_exe = b.addExecutable(.{
        .name = "pegz",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/pegz/main.zig"),
            .target = b.graph.host,
            .optimize = optimize,
        }),
    });
    pegz_exe.root_module.addImport("pegz_common", common_module);
    // Add bootstrap as a dependency so pegz can reuse its parser
    pegz_exe.root_module.addImport("bootstrap", bootstrap_mod);

    // Ensure src/pegz/parser.zig is generated before compiling pegz_exe
    pegz_exe.step.dependOn(&generate_pegz_parser.step);
    b.installArtifact(pegz_exe);

    // -------------------- Final Build Steps --------------------
    const install_step = b.getInstallStep();
    install_step.dependOn(&generate_prepegz_parser.step);
    install_step.dependOn(&prepegz_exe.step);
    install_step.dependOn(&generate_pegz_parser.step);
    install_step.dependOn(&pegz_exe.step);

    // -------------------- Testing --------------------
    const test_step = b.step("test", "Run all tests");

    // Test bootstrap scanner
    const scanner_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/scanner_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    scanner_test_module.addImport("pegz_common", common_module);
    scanner_test_module.addImport("bootstrap", bootstrap_mod);

    const scanner_tests = b.addTest(.{
        .name = "scan-test",
        .root_module = scanner_test_module,
    });
    const run_scanner_tests = b.addRunArtifact(scanner_tests);
    test_step.dependOn(&run_scanner_tests.step);

    // Test tokenizer
    const tokenizer_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/tokenizer_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    tokenizer_test_module.addImport("pegz_common", common_module);
    tokenizer_test_module.addImport("bootstrap", bootstrap_mod);

    const tokenizer_tests = b.addTest(.{
        .name = "token-test",
        .root_module = tokenizer_test_module,
    });
    const run_tokenizer_tests = b.addRunArtifact(tokenizer_tests);
    test_step.dependOn(&run_tokenizer_tests.step);

    // Test AST
    const ast_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/ast_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    ast_test_module.addImport("pegz_common", common_module);

    const ast_tests = b.addTest(.{
        .name = "ast-test",
        .root_module = ast_test_module,
    });
    const run_ast_tests = b.addRunArtifact(ast_tests);
    test_step.dependOn(&run_ast_tests.step);

    // Test common interpreter (embedded test)
    const interpreter_test_module = b.createModule(.{
        .root_source_file = b.path("src/common/interpreter.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    interpreter_test_module.addImport("pegz_common", common_module);

    const interpreter_tests = b.addTest(.{
        .name = "interpreter-test",
        .root_module = interpreter_test_module,
    });
    const run_interpreter_tests = b.addRunArtifact(interpreter_tests);
    test_step.dependOn(&run_interpreter_tests.step);

    // Test unit interpreter_test
    const unit_interpreter_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/interpreter_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    unit_interpreter_test_module.addImport("pegz_common", common_module);

    const unit_interpreter_tests = b.addTest(.{
        .name = "unit-interpreter-test",
        .root_module = unit_interpreter_test_module,
    });
    const run_unit_interpreter_tests = b.addRunArtifact(unit_interpreter_tests);
    test_step.dependOn(&run_unit_interpreter_tests.step);

    // Test unit builder_test
    const unit_builder_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/builder_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    unit_builder_test_module.addImport("pegz_common", common_module);

    const unit_builder_tests = b.addTest(.{
        .name = "unit-builder-test",
        .root_module = unit_builder_test_module,
    });
    const run_unit_builder_tests = b.addRunArtifact(unit_builder_tests);
    test_step.dependOn(&run_unit_builder_tests.step);

    // Test unit scc_test
    const unit_scc_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/scc_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    unit_scc_test_module.addImport("pegz_common", common_module);

    const unit_scc_tests = b.addTest(.{
        .name = "unit-scc-test",
        .root_module = unit_scc_test_module,
    });
    const run_unit_scc_tests = b.addRunArtifact(unit_scc_tests);
    test_step.dependOn(&run_unit_scc_tests.step);

    // Test unit left_recursion_test
    const unit_left_recursion_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/left_recursion_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    unit_left_recursion_test_module.addImport("pegz_common", common_module);

    const unit_left_recursion_tests = b.addTest(.{
        .name = "unit-left-recursion-test",
        .root_module = unit_left_recursion_test_module,
    });
    const run_unit_left_recursion_tests = b.addRunArtifact(unit_left_recursion_tests);
    test_step.dependOn(&run_unit_left_recursion_tests.step);

    // Test unit ast_optimize_test
    const unit_ast_optimize_test_module = b.createModule(.{
        .root_source_file = b.path("tests/unit/ast_optimize_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    unit_ast_optimize_test_module.addImport("pegz_common", common_module);
    const unit_ast_optimize_tests = b.addTest(.{
        .name = "unit-ast-optimize-test",
        .root_module = unit_ast_optimize_test_module,
    });
    const run_unit_ast_optimize_tests = b.addRunArtifact(unit_ast_optimize_tests);
    test_step.dependOn(&run_unit_ast_optimize_tests.step);

    // Test integration parser_test
    const integration_test_module = b.createModule(.{
        .root_source_file = b.path("tests/integration/parser_test.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    integration_test_module.addImport("pegz_common", common_module);

    const integration_tests = b.addTest(.{
        .name = "integration-test",
        .root_module = integration_test_module,
    });
    const run_integration_tests = b.addRunArtifact(integration_tests);
    test_step.dependOn(&run_integration_tests.step);
}
