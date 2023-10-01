const std = @import("std");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const typer = fluent.typer;
const ssa = fluent.ssa;

pub const RenderError =
    std.fmt.AllocPrintError ||
    std.fmt.BufPrintError ||
    blox.Error;

const span = blox.BoxOptions{ .direction = .right };

const theme = struct {
    const c = blox.Color.init;
    const meta = c(.bright, .cyan);
    const opcode = c(.bright, .yellow);
    const data = c(.bright, .magenta);
};

fn renderConstant(
    mason: *blox.Mason,
    func: *const ssa.Func,
    constant: ssa.Constant.Ref,
) RenderError!blox.Div {
    const ally = mason.ally;
    const allocPrint = std.fmt.allocPrint;

    const text = switch (func.constants.get(constant).*) {
        .unit => try allocPrint(ally, "()", .{}),
        .bool => |b| try allocPrint(ally, "{}", .{b}),
        inline .uint, .float => |n| try allocPrint(ally, "{d}", .{n}),
    };
    defer ally.free(text);

    return try mason.newPre(text, .{ .fg = theme.data });
}

fn renderLocal(
    mason: *blox.Mason,
    func: *const ssa.Func,
    local: ssa.Local,
) RenderError!blox.Div {
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, " {%}", .{local});

    return try mason.newBox(&.{
        try typer.render(mason, func.locals.get(local).*),
        try mason.newPre(text, .{}),
    }, span);
}

fn renderBlockRef(mason: *blox.Mason, ref: ssa.Block.Ref) RenderError!blox.Div {
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, "{@}", .{ref});
    return try mason.newPre(text, .{ .fg = theme.meta });
}

fn renderOp(
    mason: *blox.Mason,
    func: *const ssa.Func,
    op: ssa.Op,
) RenderError!blox.Div {
    const comma = try mason.newPre(", ", .{});

    var buf: [64]u8 = undefined;
    const code_text = try std.fmt.bufPrint(&buf, "{s} ", .{@tagName(op.code)});
    const code_tag = try mason.newPre(code_text, .{ .fg = theme.opcode });

    const code = switch (op.code) {
        .constant => |c| try renderConstant(mason, func, c),

        .branch => |br| try mason.newBox(&.{
            code_tag,
            try renderLocal(mason, func, br.cond),
            comma,
            try renderBlockRef(mason, br.if_true),
            comma,
            try renderBlockRef(mason, br.if_false),
        }, span),

        .add,
        .sub,
        .mul,
        .div,
        .mod,
        => |data| try mason.newBox(&.{
            code_tag,
            try renderLocal(mason, func, data[0]),
            comma,
            try renderLocal(mason, func, data[1]),
        }, span),
    };

    return try mason.newBox(&.{
        try renderLocal(mason, func, op.dest),
        try mason.newPre(" = ", .{}),
        code,
    }, span);
}

fn renderBlock(
    mason: *blox.Mason,
    func: *const ssa.Func,
    ref: ssa.Block.Ref,
) RenderError!blox.Div {
    const block = func.blocks.get(ref);

    var divs = std.ArrayList(blox.Div).init(mason.ally);
    defer divs.deinit();

    if (block.phi) |phi| {
        try divs.append(try mason.newBox(&.{
            try mason.newPre("phi", .{ .fg = theme.meta }),
            try mason.newSpacer(1, 1, .{}),
            try renderLocal(mason, func, phi),
        }, span));
    }

    for (block.ops) |op| {
        const div = try renderOp(mason, func, op);
        try divs.append(div);
    }

    // ret
    try divs.append(try mason.newBox(&.{
        try mason.newPre("ret", .{ .fg = theme.meta }),
        try mason.newSpacer(1, 1, .{}),
        try renderLocal(mason, func, block.ret),
    }, span));

    // stack it
    const label = try renderBlockRef(mason, ref);
    const indent = try mason.newSpacer(2, 0, .{});

    return try mason.newBox(&.{
        label,
        try mason.newBox(&.{
            indent,
            try mason.newBox(divs.items, .{}),
        }, span),
    }, .{});
}

fn renderFunc(
    mason: *blox.Mason,
    prog: *const ssa.Program,
    ref: ssa.Func.Ref,
) RenderError!blox.Div {
    const func = prog.funcs.get(ref);

    var block_divs = std.ArrayList(blox.Div).init(mason.ally);
    defer block_divs.deinit();

    try block_divs.append(try mason.newBox(&.{
        try mason.newPre("enter ", .{ .fg = theme.meta }),
        try renderBlockRef(mason, func.entry),
    }, span));

    var blocks = func.blocks.iterator();
    while (blocks.nextEntry()) |entry| {
        const div = try renderBlock(mason, func, entry.ref);
        try block_divs.append(div);
    }

    // stack it
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, "{func}", .{ref});

    const label = try mason.newPre(text, .{ .fg = theme.meta });
    const indent = try mason.newSpacer(2, 0, .{});

    return try mason.newBox(&.{
        label,
        try mason.newBox(&.{
            indent,
            try mason.newBox(block_divs.items, .{}),
        }, span),
    }, .{});
}

pub fn renderProgram(
    prog: *const ssa.Program,
    mason: *blox.Mason,
) RenderError!blox.Div {
    var func_divs = std.ArrayList(blox.Div).init(mason.ally);
    defer func_divs.deinit();

    var funcs = prog.funcs.iterator();
    while (funcs.nextEntry()) |entry| {
        const div = try renderFunc(mason, prog, entry.ref);
        try func_divs.append(div);
    }

    return try mason.newBox(func_divs.items, .{});
}
