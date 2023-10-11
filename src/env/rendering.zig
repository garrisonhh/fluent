const std = @import("std");
const blox = @import("blox");
const env = @import("env.zig");

const theme = struct {
    const c = blox.Color.init;
    const ident = c(.normal, .red);
};

const span = blox.BoxOptions{ .direction = .right };

pub fn renderIdent(mason: *blox.Mason, ident: env.Ident) blox.Error!blox.Div {
    return try mason.newPre(env.identStr(ident), .{ .fg = theme.ident });
}

pub fn renderName(mason: *blox.Mason, name: env.Name) blox.Error!blox.Div {
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
