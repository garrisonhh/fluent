const std = @import("std");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const typer = fluent.typer;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const color = blox.Color.init(.normal, .green);

fn renderType(mason: *blox.Mason, t: Type) RenderError!blox.Div {
    return switch (t) {
        inline .any,
        .unit,
        .ident,
        .bool,
        .int,
        .float,
        => |_, tag| try mason.newPre(@tagName(tag), .{ .fg = color }),
    };
}

pub fn renderTypeId(mason: *blox.Mason, t: Type.Id) RenderError!blox.Div {
    return try renderType(mason, typer.get(t).*);
}
