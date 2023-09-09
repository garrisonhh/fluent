const std = @import("std");
const Lexer = @import("Lexer.zig");

fn debugLex(text: []const u8, writer: anytype) !void {
    var lexer = Lexer.init(text);
    while (try lexer.nextToken()) |token| {
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
    ;

    try debugLex(text, stdout);

    try bw.flush();
}
