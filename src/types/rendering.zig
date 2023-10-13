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

const span = blox.BoxOptions{ .direction = .right };

/// TODO this needs to be refactored with env typenames in mind
pub fn renderType(mason: *blox.Mason, t: Type, this: Type.Id) RenderError!blox.Div {
    const ally = mason.ally;
    return switch (t) {
        inline .unit,
        .type,
        .name,
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
                try divs.append(try renderTypeId(mason, param));
            }

            try divs.append(try mason.newPre(") -> ", .{}));
            try divs.append(try renderTypeId(mason, f.returns));

            break :f try mason.newBox(divs.items, span);
        },
        .class => try mason.newPre("class", .{ .fg = theme.t }),
        .@"struct" => |st| st: {
            const comma = try mason.newPre(", ", .{});

            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            try divs.append(try mason.newPre("struct", .{ .fg = theme.t }));
            try divs.append(try mason.newPre("(", .{}));

            for (st.fields, 0..) |field, i| {
                const field_type = ft: {
                    if (field.eql(this)) {
                        break :ft try mason.newPre("self", .{ .fg = theme.t });
                    } else {
                        break :ft try renderTypeId(mason, field);
                    }
                };

                if (i > 0) try divs.append(comma);
                try divs.append(field_type);
            }

            try divs.append(try mason.newPre(")", .{}));

            break :st try mason.newBox(divs.items, span);
        },
    };
}

pub fn renderTypeId(mason: *blox.Mason, t: Type.Id) RenderError!blox.Div {
    return try renderType(mason, typer.get(t).*, t);
}
