const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "l3p",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);
    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_tests = b.addTest(.{
        .root_module = exe.root_module,
    });

    const run_exe_tests = b.addRunArtifact(exe_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_exe_tests.step);

    // --- snapshot tests
    const snap_test_step = b.step("test-snapshot", "Run snapshot tests");
    try testSnapshot(b, snap_test_step, exe);
}

fn testSnapshot(b: *std.Build, main_step: *std.Build.Step, exe: *std.Build.Step.Compile) !void {
    const test_output_dir = "tests/snapshots/";

    var tests_dir = try std.fs.cwd().openDir("tests", .{ .iterate = true });
    defer tests_dir.close();

    var it = tests_dir.iterate();
    while (try it.next()) |entry| {
        const file_name = getFileName(entry.name);
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".l3p")) continue;

        // build exe
        const build_test_exe = b.addRunArtifact(exe);
        build_test_exe.addArgs(&.{
            "--",
            b.fmt("tests/{s}", .{entry.name}),
            "--target=linux-x64",
        });
        build_test_exe.setName(b.fmt("Compile test: {s}", .{entry.name}));

        // run exe
        const run_test_exe = b.addSystemCommand(&.{b.fmt("./l3p-out/{s}", .{file_name})});
        run_test_exe.setName(b.fmt("Running test: {s}", .{file_name}));
        run_test_exe.step.dependOn(&build_test_exe.step);
        run_test_exe.has_side_effects = true;

        const output = run_test_exe.captureStdOut();

        // write exe output
        const run_test_inst = b.addInstallFileWithDir(output, .{ .custom = "../" ++ test_output_dir }, b.fmt(
            "{s}.txt",
            .{file_name},
        ));
        run_test_inst.step.dependOn(&run_test_exe.step);

        // diff output
        const diff = b.addSystemCommand(&.{
            "git",
            "diff",
            "--exit-code",
        });
        diff.setName(b.fmt("Comparing snapshot: {s}", .{file_name}));
        diff.addDirectoryArg(b.path(b.fmt(
            test_output_dir ++ "{s}.txt",
            .{file_name},
        )));
        diff.step.dependOn(&run_test_inst.step);

        main_step.dependOn(&diff.step);
    }
}

fn getFileName(path: []const u8) []const u8 {
    var file_name = std.fs.path.basename(path);
    const index = std.mem.lastIndexOfScalar(u8, file_name, '.') orelse 0;
    file_name = if (index == 0) file_name else file_name[0..index];
    return file_name;
}
