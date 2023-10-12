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

        .function => |func| func: {
            const lowered = func.ssa != null;
            const status = try mason.newPre(
                if (lowered) "lowered" else "unlowered",
                .{
                    .fg = blox.Color.init(
                        .normal,
                        if (lowered) .green else .red,
                    ),
                },
            );

            break :func try mason.newBox(&.{
                try mason.newPre("fn", .{ .fg = theme.syntax }),
                try mason.newPre(" <", .{}),
                try typer.render(mason, func.type),
                try mason.newPre("> (", .{}),
                status,
                try mason.newPre(")", .{}),
            }, span);
        },
    };
}

pub fn renderValueRef(mason: *blox.Mason, ref: Value.Ref) blox.Error!blox.Div {
    return try renderValue(mason, env.get(ref).*);
}

pub fn renderEnv(mason: *blox.Mason) blox.Error!blox.Div {
    const ally = mason.ally;

    const space = try mason.newSpacer(1, 1, .{});
    const indent = try mason.newSpacer(2, 1, .{});

    var divs = std.ArrayList(blox.Div).init(ally);
    defer divs.deinit();

    var def_iter = env.defs.iterator();
    while (def_iter.next()) |entry| {
        const name = entry.key_ptr.*;
        const ref = entry.value_ptr.*;

        const name_div = try renderName(mason, name);
        const value_div = try renderValueRef(mason, ref);

        const entry_div = switch (mason.getSize(value_div)[1]) {
            0...1 => try mason.newBox(&.{
                name_div,
                space,
                value_div,
            }, span),
            else => try mason.newBox(&.{
                name_div,
                try mason.newBox(&.{ indent, value_div }, span),
            }, .{}),
        };

        try divs.append(entry_div);
    }

    return try mason.newBox(divs.items, .{});
}