const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

fn debugParse(ally: Allocator, source: fluent.Source, writer: anytype) !void {
    var ast = try fluent.parse(ally, source);
    defer ast.deinit(ally);

    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    const rendered = try ast.render(&mason);

    try writer.print("[ast]\n", .{});
    try mason.write(rendered, writer, .{});
    try writer.print("\n", .{});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    fluent.init();
    defer fluent.deinit(ally);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const text =
        \\def a
        \\  add 123 456
        \\
        \\def b
        \\  sub 23.1232 (negate 10.) (((3))) ()
        \\
        \\def c
        \\  1;
        \\  1 + 2;
        \\  1 * 2 + 3;
        \\  1 % 3 + 2
        \\
        \\def d
        \\  f 1 2 3 * g 4 5 6;
        \\  ()
        \\
        \\def e
        \\  &&std::os::linux.read 4096 &buf;
        \\  &(&fun) arg1 arg2
    ;
    const source = try fluent.sources.add(ally, "test", text);

    try debugParse(ally, source, stdout);

    try bw.flush();
}
