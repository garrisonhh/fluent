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
};

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

fn renderFuncName(
    mason: *blox.Mason,
    prog: *const ssa.Program,
    ref: ssa.Func.Ref,
) RenderError!blox.Div {
    const name = prog.funcs.get(ref).name;
    return try fluent.env.renderName(mason, name);
}

fn renderOp(
    mason: *blox.Mason,
    prog: *const ssa.Program,
    func: *const ssa.Func,
    op: ssa.Op,
) RenderError!blox.Div {
    const comma = try mason.newPre(", ", .{});

    var buf: [64]u8 = undefined;
    const code_text = try std.fmt.bufPrint(&buf, "{s} ", .{@tagName(op.code)});
    const code_tag = try mason.newPre(code_text, .{ .fg = theme.opcode });

    const code = switch (op.code) {
        .constant => |v| try fluent.env.renderValue(mason, fluent.env.get(v).*),

        .branch => |br| try mason.newBox(&.{
            code_tag,
            try renderLocal(mason, func, br.cond),
            comma,
            try renderBlockRef(mason, br.if_true),
            comma,
            try renderBlockRef(mason, br.if_false),
        }, span),

        .call => |call| call: {
            var divs = std.ArrayList(blox.Div).init(mason.ally);
            defer divs.deinit();

            try divs.appendSlice(&.{
                code_tag,
                try renderFuncName(mason, prog, call.func),
            });

            for (call.args, 0..) |arg, i| {
                if (i > 0) try divs.append(comma);
                try divs.append(try renderLocal(mason, func, arg));
            }

            break :call try mason.newBox(divs.items, span);
        },

        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .eq,
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
    prog: *const ssa.Program,
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
        const div = try renderOp(mason, prog, func, op);
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
        const div = try renderBlock(mason, prog, func, entry.ref);
        try block_divs.append(div);
    }

    const label = try mason.newBox(&.{
        try mason.newPre("fn", .{ .fg = theme.meta }),
        try mason.newSpacer(1, 1, .{}),
        try renderFuncName(mason, prog, ref),
        try mason.newPre(" <", .{}),
        try fluent.typer.render(mason, func.type),
        try mason.newPre(">", .{}),
    }, span);

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
