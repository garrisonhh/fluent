const std = @import("std");
const builtin = @import("builtin");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Loc = fluent.Loc;

const theme = struct {
    const c = blox.Color.init;

    const err = c(.bright, .black);
};

const span = blox.BoxOptions{ .direction = .right };

/// adds tag to error description
fn errorDesc(
    mason: *blox.Mason,
    desc: blox.Div,
) blox.Error!blox.Div {
    return try mason.newBox(&.{
        try mason.newPre("[", .{}),
        try mason.newPre("syntax error", .{ .fg = theme.err }),
        try mason.newPre("] ", .{}),
        desc,
    }, span);
}

/// an error with a description and a location
fn simpleError(
    mason: *blox.Mason,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype,
) blox.Error!blox.Div {
    const ally = mason.ally;
    const text = try std.fmt.allocPrint(ally, fmt, args);
    defer ally.free(text);

    return try mason.newBox(&.{
        try errorDesc(mason, try mason.newPre(text, .{})),
        try loc.render(mason),
    }, .{});
}

pub fn renderSyntaxError(
    meta: fluent.SyntaxErrorMeta,
    mason: *blox.Mason,
) blox.Error!blox.Div {
    return switch (meta) {
        .unexpected_eof => |loc| try simpleError(
            mason,
            loc,
            "unexpected EOF",
            .{},
        ),

        .invalid_literal => |il| try simpleError(
            mason,
            il.loc,
            "invalid {s} literal",
            .{@tagName(il.tag)},
        ),

        .expected_op_expr => |exp| try simpleError(
            mason,
            exp.loc,
            "expected expression for operator `{s}`",
            .{exp.operator},
        ),

        .expected_token => |exp| try simpleError(
            mason,
            exp.loc,
            "expected {s}",
            .{@tagName(exp.tag)},
        ),

        .expected_one_of => |exp| e: {
            var list = std.ArrayList(u8).init(mason.ally);
            defer list.deinit();
            const writer = list.writer();

            std.debug.assert(exp.tags.len >= 2);

            const tags = exp.tags;

            for (tags[0 .. tags.len - 2]) |tag| {
                try writer.print("{s}, ", .{@tagName(tag)});
            }

            try writer.print("{s} or {s}", .{
                @tagName(tags[tags.len - 2]),
                @tagName(tags[tags.len - 1]),
            });

            break :e try simpleError(
                mason,
                exp.loc,
                "expected one of {s}",
                .{list.items},
            );
        },

        .expected_desc => |exp| try simpleError(
            mason,
            exp.loc,
            "expected {s}",
            .{exp.desc},
        ),
    };
}
