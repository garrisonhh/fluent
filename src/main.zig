const std = @import("std");
const Lexer = @import("Lexer.zig");
const parser = @import("parser.zig");

fn debugLex(text: []const u8, writer: anytype) !void {
    var lexer = Lexer.init(text);
    while (try lexer.next()) |token| {
        try writer.print(
            "{s:<12} `{s}`\n",
            .{@tagName(token.tag), lexer.slice(token)},
        );
    }
}

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    const text =
        \\hello (a b c)
        \\123 -123 +123 123.456 -123.456 +123.456
    ;

    try debugLex(text, stdout);

    try bw.flush();
}
