const std = @import("std");
const blox = @import("blox");
const Type = @import("type.zig").Type;
const typer = @import("typer.zig");

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const color = blox.Color.init(.normal, .green);

pub fn renderType(mason: *blox.Mason, t: Type) RenderError!blox.Div {
    return switch (t) {
        inline .unit,
        .bool,
        .anyint,
        .anyfloat,
        => |_, tag| try mason.newPre(@tagName(tag), .{ .fg = color }),
    };
}

pub fn renderTypeId(mason: *blox.Mason, t: Type.Id) RenderError!blox.Div {
    return try renderType(mason, typer.get(t).*);
}