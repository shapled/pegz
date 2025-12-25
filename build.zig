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
        .root_source_file = b.path("src/bootstrap/parser.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });
    bootstrap_mod.addImport("pegz_common", common_module);
    prepegz_exe.root_module.addImport("bootstrap", bootstrap_mod);


    // Ensure prepegz/grammar.zig is generated before compiling prepegz_exe
    prepegz_exe.step.dependOn(&generate_prepegz_parser.step);
    b.installArtifact(prepegz_exe);

    // -------------------- Stage 3: Generate pegz/parser.zig --------------------
    // This step would use grammars/pegz.pegz, but we use prepegz.pegz as placeholder
    const generate_pegz_parser = b.addRunArtifact(prepegz_exe);

    // If you have pegz.pegz, replace ./grammars/prepegz.pegz with ./grammars/pegz.pegz
    generate_pegz_parser.addArgs(&[_][]const u8{
        "./grammars/prepegz.pegz",  // Replace with ./grammars/pegz.pegz when available
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
}
