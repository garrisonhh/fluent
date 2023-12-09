const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

/// attempts to compile source, returning success
fn debugCompile(ally: Allocator, source: fluent.Source, writer: anytype) !bool {
    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    // print source
    const file = fluent.sources.get(source);
    try writer.print("[src]\n{s}\n", .{file.text});

    // parse
    var ast = fluent.Ast.init(ally);
    defer ast.deinit();

    var parse_ebuf = fluent.SyntaxErrorBuf.init(ally);
    defer parse_ebuf.deinit();

    const parse_res = fluent.parse(&ast, &parse_ebuf, source);
    const root = switch (try parse_ebuf.filter(parse_res)) {
        .payload => |x| x,
        .err => |meta| {
            const rendered = try meta.render(&mason);
            try mason.write(rendered, stderr, .{});
            return false;
        },
    };

    // analyze
    var analyze_ebuf = fluent.TypeErrorBuf.init(ally);
    defer analyze_ebuf.deinit();

    const analyze_res = fluent.analyze(&ast, &analyze_ebuf, file.name, root);
    switch (try analyze_ebuf.filter(analyze_res)) {
        .payload => |x| x,
        .err => |meta| {
            const rendered = try meta.render(&mason);
            try mason.write(rendered, stderr, .{});
            return false;
        },
    }

    // render ast
    const ast_div = try ast.render(&mason, root);

    try writer.print("[ast]\n", .{});
    try mason.write(ast_div, writer, .{});
    try writer.print("\n", .{});
    try writer.context.flush();

    // lower to ssa
    var ssa_object = try fluent.lower(ally, &ast, file.name, root);
    defer ssa_object.deinit(ally);

    // render ssa
    const ssa_div = try ssa_object.render(&mason);

    try writer.print("[ssa]\n", .{});
    try mason.write(ssa_div, writer, .{});
    try writer.print("\n", .{});
    try writer.context.flush();

    // jit assemble ssa
    try fluent.assemble(ally, ssa_object);

    return true;
}

pub fn main() !void {
    // init
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 20,
    }){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    try fluent.init();
    defer fluent.deinit();

    // test source
    const text =
        \\fn f(a: i64, b: i64) i64 ->
        \\  a + b
        \\
    ;
    const source = try fluent.sources.add("test", text);

    // debug compile
    const stdout_writer = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_writer);
    const stdout = bw.writer();

    if (try debugCompile(ally, source, stdout)) {
        // attempt to run the function
        const func_name = try fluent.env.nameFromStr("f");
        const F = fn (i64, i64) callconv(.SysV) i64;
        const func = fluent.env.getCompiled(func_name, F).?;
        const res = func(32, 31);

        try stdout.print("function returned {}\n", .{res});
    }

    try bw.flush();
}
