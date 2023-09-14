const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

fn debugParse(ally: Allocator, source: fluent.Source, writer: anytype) !void {
    // parse
    var ast = fluent.Ast{};
    defer ast.deinit(ally);

    ast.root = fluent.parse(ally, &ast, source, .program) catch |e| switch (e) {
        fluent.ParseError.InvalidSyntax => {
            var mason = blox.Mason.init(ally);
            defer mason.deinit();

            for (ast.getErrors()) |err| {
                const rendered = try err.render(&mason);
                try mason.write(rendered, stderr, .{});
            }

            return;
        },
        else => {
            return e;
        },
    };

    // render
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

    try fluent.init(ally);
    defer fluent.deinit(ally);

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const text =
        \\let a = 123 - 456
        \\let b = &(&fun) arg1 -arg2
        \\
        \\let c =
        \\  1 * 2 + 3;
        \\  1 % 3 + 2
        \\
        \\let d =
        \\  if true then
        \\    f 3
        \\  else
        \\    a 5
    ;
    const source = try fluent.sources.add(ally, "test", text);

    try debugParse(ally, source, stdout);

    try bw.flush();
}
