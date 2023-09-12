const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const common = b.dependency("zighh", .{}).module("common");
    const blox = b.dependency("blox", .{}).module("blox");

    // build options
    const log_lexer = b.option(bool, "log-lexer", "enable lexer logging");

    // propagated options
    const options = b.addOptions();
    options.addOption(bool, "log_lexer", log_lexer orelse false);

    // exe
    const exe = b.addExecutable(.{
        .name = "fluent-parser",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.addOptions("options", options);
    exe.addModule("common", common);
    exe.addModule("blox", blox);

    b.installArtifact(exe);

    // runner
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "run the app");
    run_step.dependOn(&run_cmd.step);

    // test runner
    const tests = b.addTest(.{
        .root_source_file = .{ .path = "src/tests.zig" },
        .target = target,
        .optimize = optimize,
    });

    tests.addModule("common", common);
    tests.addModule("blox", blox);

    const test_cmd = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&test_cmd.step);
}
