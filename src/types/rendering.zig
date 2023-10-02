const std = @import("std");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const typer = fluent.typer;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const color = blox.Color.init(.normal, .green);

fn renderType(mason: *blox.Mason, t: Type) RenderError!blox.Div {
    const ally = mason.ally;
    return switch (t) {
        inline .unit,
        .ident,
        .bool,
        => |_, tag| try mason.newPre(@tagName(tag), .{ .fg = color }),

        .int => |meta| int: {
            const sign_ch: u8 = switch (meta.signedness) {
                .signed => 'i',
                .unsigned => 'u',
            };

            const str = try std.fmt.allocPrint(ally, "{c}{s}", .{
                sign_ch,
                @tagName(meta.bits),
            });
            defer ally.free(str);

            break :int try mason.newPre(str, .{ .fg = color });
        },

        .float => |meta| float: {
            const str = try std.fmt.allocPrint(ally, "f{s}", .{
                @tagName(meta.bits),
            });
            defer ally.free(str);

            break :float try mason.newPre(str, .{ .fg = color });
        },
    };
}

pub fn renderTypeId(mason: *blox.Mason, t: Type.Id) RenderError!blox.Div {
    return try renderType(mason, typer.get(t).*);
}
