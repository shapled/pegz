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
    
    const lldb = b.addSystemCommand(&.{
        "lldb",
    });
    // appends the bootstrap executable path to the lldb command line
    lldb.addArtifactArg(bootstrap_exe);
    // add test.pegz as argument to bootstrap executable
    lldb.addArgs(&.{
        "--",
        "test.pegz",
    });

    const lldb_step = b.step("debug-bootstrap", "run the bootstrap under lldb with test.pegz");
    lldb_step.dependOn(&lldb.step);
}
