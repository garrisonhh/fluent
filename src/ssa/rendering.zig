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
    const label = c(.bright, .cyan);
    const opcode = c(.bright, .yellow);
    const data = c(.bright, .magenta);
};

fn renderConstant(
    mason: *blox.Mason,
    prog: *const ssa.Program,
    constant: ssa.Constant.Ref,
) RenderError!blox.Div {
    const ally = mason.ally;
    const allocPrint = std.fmt.allocPrint;

    const text = switch (prog.constants.get(constant).*) {
        .unit => try allocPrint(ally, "()", .{}),
        .bool => |b| try allocPrint(ally, "{}", .{b}),
        inline .uint, .float => |n| try allocPrint(ally, "{d}", .{n}),
    };
    defer ally.free(text);

    return try mason.newPre(text, .{ .fg = theme.data });
}

fn renderLocal(
    mason: *blox.Mason,
    list: *const ssa.LocalList,
    local: ssa.Local,
) RenderError!blox.Div {
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, " {%}", .{local});

    return try mason.newBox(&.{
        try typer.render(mason, list.get(local).*),
        try mason.newPre(text, .{}),
    }, span);
}

fn renderBlockRef(mason: *blox.Mason, ref: ssa.Block.Ref) RenderError!blox.Div {
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, "{@}", .{ref});
    return try mason.newPre(text, .{ .fg = theme.label });
}

fn renderOp(
    mason: *blox.Mason,
    prog: *const ssa.Program,
    local_list: *const ssa.LocalList,
    op: ssa.Op,
) RenderError!blox.Div {
    const space = try mason.newPre(" ", .{});
    const comma = try mason.newPre(", ", .{});

    var buf: [64]u8 = undefined;
    const code_text = try std.fmt.bufPrint(&buf, "{s} ", .{@tagName(op.code)});
    const code_tag = try mason.newPre(code_text, .{ .fg = theme.opcode });

    const code = switch (op.code) {
        .constant => |c| try renderConstant(mason, prog, c),

        .branch => |br| try mason.newBox(&.{
            code_tag,
            try renderLocal(mason, local_list, br.cond),
            space,
            try renderBlockRef(mason, br.if_true),
            comma,
            try renderBlockRef(mason, br.if_true),
        }, .{}),

        .add,
        .sub,
        .mul,
        .div,
        .mod,
        => |data| try mason.newBox(&.{
            code_tag,
            try renderLocal(mason, local_list, data[0]),
            comma,
            try renderLocal(mason, local_list, data[1]),
        }, span),
    };

    return try mason.newBox(&.{
        try renderLocal(mason, local_list, op.dest),
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

    // TODO phi

    for (block.ops) |op| {
        const div = try renderOp(mason, prog, &func.locals, op);
        try divs.append(div);
    }

    // ret
    try divs.append(try mason.newBox(&.{
        try mason.newPre("returns", .{ .fg = theme.label }),
        try mason.newSpacer(1, 1, .{}),
        try renderLocal(mason, &func.locals, block.ret),
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

    var blocks = func.blocks.iterator();
    while (blocks.nextEntry()) |entry| {
        const div = try renderBlock(mason, prog, func, entry.ref);
        try block_divs.append(div);
    }

    // stack it
    var buf: [64]u8 = undefined;
    const text = try std.fmt.bufPrint(&buf, "{func}", .{ref});

    const label = try mason.newPre(text, .{ .fg = theme.label });
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
