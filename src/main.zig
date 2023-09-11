const std = @import("std");
const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");
const blox = @import("blox");

fn debugLex(text: []const u8, writer: anytype) !void {
    var lexer = Lexer.init(text);

    try writer.print("[tokens]\n", .{});
    while (try lexer.next()) |token| {
        try writer.print(
            "{s:<12} `{s}`\n",
            .{ @tagName(token.tag), lexer.slice(token) },
        );
    }

    try writer.print("\n", .{});
}

fn debugParse(text: []const u8, writer: anytype) !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const ally = gpa.allocator();

    var ast = try parser.parse(ally, text);
    defer ast.deinit(ally);

    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    const rendered = try ast.render(&mason);

    try writer.print("[ast]\n", .{});
    try mason.write(rendered, writer, .{});
    try writer.print("\n", .{});
}

pub fn main() !void {
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
        \\  &&std::os::linux.read 1 4096 &buf
    ;

    try debugLex(text, stdout);
    try bw.flush();

    try debugParse(text, stdout);
    try bw.flush();
}
