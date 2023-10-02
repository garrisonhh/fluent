const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

fn debugParse(ally: Allocator, source: fluent.Source, writer: anytype) !void {
    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    // parse
    var ast = fluent.Ast.init(ally);
    defer ast.deinit();

    const root = switch (try fluent.parse(&ast, source, .program)) {
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
    var ssa_prog = fluent.ssa.Program.init(ally);
    defer ssa_prog.deinit();

    try fluent.lower(&ast, &ssa_prog, root, .program);

    // render ssa
    const ssa_div = try ssa_prog.render(&mason);

    try writer.print("[ssa]\n", .{});
    try mason.write(ssa_div, writer, .{});
    try writer.print("\n", .{});
}

pub fn main() !void {
    // init
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    try fluent.init(ally);
    defer fluent.deinit(ally);

    // test source
    const text =
        \\let x = {} -> 1
    ;
    const source = try fluent.sources.add(ally, "test", text);

    // debug parse
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try debugParse(ally, source, stdout);

    try bw.flush();
}
