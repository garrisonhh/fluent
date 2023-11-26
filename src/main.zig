const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

fn debugCompile(ally: Allocator, source: fluent.Source, writer: anytype) !void {
    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    // print source
    try writer.print("[src]\n{s}\n", .{fluent.sources.get(source).text});

    // parse
    var ast = fluent.Ast.init(ally);
    defer ast.deinit();

    const root = switch (try fluent.parse(&ast, source)) {
        .ok => |node| node,
        .fail => {
            for (ast.getErrors()) |err| {
                const rendered = try err.render(&mason);
                try mason.write(rendered, stderr, .{});
            }

            return;
        },
    };

    // analyze
    switch (try fluent.analyze(&ast, root)) {
        .ok => {},
        .fail => {
            for (ast.getErrors()) |err| {
                const rendered = try err.render(&mason);
                try mason.write(rendered, stderr, .{});
            }

            return;
        },
    }

    // render ast
    const ast_div = try ast.render(&mason, root);

    try writer.print("[ast]\n", .{});
    try mason.write(ast_div, writer, .{});
    try writer.print("\n", .{});

    // lower to ssa
    var ssa_object = try fluent.lower(ally, &ast, root);
    defer ssa_object.deinit(ally);

    // render ssa
    const ssa_div = try ssa_object.render(&mason);

    try writer.print("[ssa]\n", .{});
    try mason.write(ssa_div, writer, .{});
    try writer.print("\n", .{});

    // jit assemble ssa
    try writer.context.flush();
    try fluent.assemble(ally, ssa_object);
}

pub fn main() !void {
    // init
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 20,
    }){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    try fluent.init(ally);
    defer fluent.deinit(ally);

    // test source
    const text =
        \\fn useless {} u64 -> 42
        \\
    ;
    const source = try fluent.sources.add(ally, "test", text);

    // debug compile
    const stdout_writer = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_writer);
    const stdout = bw.writer();

    try debugCompile(ally, source, stdout);

    // attempt to run the function
    const func_name = try fluent.env.nameFromStr("useless");
    const F = fn() callconv(.SysV) i32;
    const func = fluent.env.getCompiled(func_name, F).?;
    const res = func();

    try stdout.print("function returned {}\n", .{res});

    try bw.flush();
}
