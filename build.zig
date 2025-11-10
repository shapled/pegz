const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const common_module = b.createModule(.{
        .root_source_file = b.path("src/common/mod.zig"),
        .target = b.graph.host,
        .optimize = optimize,
    });

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
}
