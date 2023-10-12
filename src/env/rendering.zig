const std = @import("std");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ident = fluent.Ident;
const Name = fluent.Name;
const Value = fluent.Value;
const env = fluent.env;
const typer = fluent.typer;

const theme = struct {
    const c = blox.Color.init;
    const syntax = c(.normal, .cyan);
    const ident = c(.normal, .red);
    const data = c(.normal, .magenta);
};

const span = blox.BoxOptions{ .direction = .right };

pub fn renderIdent(mason: *blox.Mason, ident: Ident) blox.Error!blox.Div {
    return try mason.newPre(env.identStr(ident), .{ .fg = theme.ident });
}

pub fn renderName(mason: *blox.Mason, name: Name) blox.Error!blox.Div {
    const dot = try mason.newPre(".", .{});

    const idents = env.nameSlice(name);
    if (idents.len == 0) return dot;

    var divs = std.ArrayList(blox.Div).init(mason.ally);
    defer divs.deinit();

    for (idents, 0..) |ident, i| {
        if (i > 0) try divs.append(dot);
        try divs.append(try renderIdent(mason, ident));
    }

    return mason.newBox(divs.items, span);
}

pub fn renderValue(mason: *blox.Mason, value: Value) blox.Error!blox.Div {
    const ally = mason.ally;
    return switch (value) {
        .unit => try mason.newPre("()", .{ .fg = theme.data }),
        .type => |t| try typer.render(mason, t),
        .name => |name| try renderName(mason, name),
        .bool => |b| bool: {
            const text = try std.fmt.allocPrint(ally, "{}", .{b});
            defer ally.free(text);

            break :bool try mason.newPre(text, .{ .fg = theme.data });
        },

        inline .int, .uint, .float => |x| switch (x) {
            inline else => |n| num: {
                const text = try std.fmt.allocPrint(ally, "{d}", .{n});
                defer ally.free(text);

                break :num try mason.newPre(text, .{ .fg = theme.data });
            },
        },

        .function => |func| try mason.newBox(&.{
            try mason.newPre("fn ", .{ .fg = theme.syntax }),
            try mason.newPre("<", .{}),
            try typer.render(mason, func.type),
            try mason.newPre(">", .{}),
        }, span),
    };
}