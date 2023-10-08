const std = @import("std");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const typer = fluent.typer;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const theme = struct {
    const t = blox.Color.init(.normal, .green);
    const field = blox.Color.init(.normal, .yellow);
};

const span = blox.BoxOptions{
    .direction = .right,
};

fn renderType(mason: *blox.Mason, t: Type, this: Type.Id) RenderError!blox.Div {
    const ally = mason.ally;
    return switch (t) {
        inline .unit,
        .type,
        .ident,
        .bool,
        => |_, tag| try mason.newPre(@tagName(tag), .{ .fg = theme.t }),

        .int => |i| int: {
            const sign_ch: u8 = switch (i.signedness) {
                .signed => 'i',
                .unsigned => 'u',
            };

            const str = try std.fmt.allocPrint(ally, "{c}{s}", .{
                sign_ch,
                @tagName(i.bits),
            });
            defer ally.free(str);

            break :int try mason.newPre(str, .{ .fg = theme.t });
        },
        .float => |f| float: {
            const str = try std.fmt.allocPrint(ally, "f{s}", .{
                @tagName(f.bits),
            });
            defer ally.free(str);

            break :float try mason.newPre(str, .{ .fg = theme.t });
        },

        .@"fn" => |f| f: {
            const comma = try mason.newPre(", ", .{});

            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            try divs.append(try mason.newPre("(", .{}));
            for (f.params, 0..) |param, i| {
                if (i > 0) try divs.append(comma);

                const param_div = try mason.newBox(&.{
                    try mason.newPre(param.name, .{ .fg = theme.field }),
                    try mason.newPre(": ", .{}),
                    try renderTypeId(mason, param.type),
                }, span);

                try divs.append(param_div);
            }

            try divs.append(try mason.newPre(") -> ", .{}));
            try divs.append(try renderTypeId(mason, f.returns));

            break :f try mason.newBox(divs.items, span);
        },
        .@"struct" => |st| st: {
            var fields = std.ArrayList(blox.Div).init(ally);
            defer fields.deinit();

            for (st.fields) |field| {
                const field_name = try mason.newBox(&.{
                    try mason.newPre(field.name, .{ .fg = theme.field }),
                    try mason.newPre(": ", .{}),
                }, span);

                const field_type = ft: {
                    if (field.type.eql(this)) {
                        break :ft try mason.newPre("self", .{ .fg = theme.t });
                    } else {
                        break :ft try renderTypeId(mason, field.type);
                    }
                };

                if (mason.getSize(field_type)[1] <= 1) {
                    try fields.append(try mason.newBox(&.{
                        field_name,
                        field_type,
                    }, span));
                } else {
                    try fields.append(try mason.newBox(&.{
                        field_name,
                        try mason.newBox(&.{
                            try mason.newSpacer(2, 0, .{}),
                            field_type,
                        }, span),
                    }, .{}));
                }
            }

            break :st try mason.newBox(&.{
                try mason.newPre("struct", .{ .fg = theme.t }),
                try mason.newBox(&.{
                    try mason.newSpacer(2, 0, .{}),
                    try mason.newBox(fields.items, .{}),
                }, span),
            }, .{});
        },
    };
}

pub fn renderTypeId(mason: *blox.Mason, t: Type.Id) RenderError!blox.Div {
    return try renderType(mason, typer.get(t).*, t);
}
