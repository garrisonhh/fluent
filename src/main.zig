const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");

pub const std_options = fluent.std_options;

fn debugParse(ally: Allocator, source: fluent.Source, writer: anytype) !void {
    // parse
    var ast = fluent.Ast.init(ally);
    defer ast.deinit();

    const root = fluent.parse(&ast, source, .program) catch |e| switch (e) {
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

    // analyze
    fluent.analyze(&ast, root) catch |e| switch (e) {
        fluent.SemaError.InvalidType => {
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

    const rendered = try ast.render(&mason, root);

    try writer.print("[ast]\n", .{});
    try mason.write(rendered, writer, .{});
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
        \\let unit = ()
        \\let chk_true = true
        \\let chk_false = false
        \\let chk_int = 123
        \\let chk_real = 123.123
        \\
        \\let a = 3 * 4. + 5.
    ;
    const source = try fluent.sources.add(ally, "test", text);

    // debug parse
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try debugParse(ally, source, stdout);

    try bw.flush();
}
