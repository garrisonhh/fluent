const std = @import("std");
const builtin = @import("builtin");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const typer = fluent.typer;
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
        try mason.newPre("type error", .{ .fg = theme.err }),
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

pub fn renderTypeError(
    meta: fluent.TypeErrorMeta,
    mason: *blox.Mason,
) blox.Error!blox.Div {
    return switch (meta) {
        .unknown_name => |unk| try mason.newBox(&.{
            try errorDesc(mason, try mason.newBox(&.{
                try mason.newPre("unknown name ", .{}),
                try fluent.env.renderName(mason, unk.name),
                try mason.newPre(" in scope ", .{}),
                try fluent.env.renderName(mason, unk.scope),
            }, span)),
            try unk.loc.render(mason),
        }, .{}),

        .expected => |exp| try mason.newBox(&.{
            try errorDesc(mason, try mason.newBox(&.{
                try mason.newPre("expected ", .{}),
                try typer.render(mason, exp.expected),
                try mason.newPre(", found ", .{}),
                try typer.render(mason, exp.found),
            }, span)),
            try exp.loc.render(mason),
        }, .{}),

        .expected_matching => |exp| try mason.newBox(&.{
            try errorDesc(mason, try mason.newBox(&.{
                try mason.newPre("expected ", .{}),
                try typer.render(mason, exp.expected),
                try mason.newPre(", found ", .{}),
                try typer.render(mason, exp.found),
            }, span)),
            try exp.found_loc.render(mason),
            try mason.newPre("this would match the expression here", .{}),
            try exp.expected_loc.render(mason),
        }, .{}),
    };
}
