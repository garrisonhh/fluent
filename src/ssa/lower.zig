const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const ssa = fluent.ssa;

pub const Error = Allocator.Error;

fn lowerExpr(
    ast: *const Ast,
    block: *ssa.BlockBuilder,
    node: Ast.Node,
) Error!ssa.Local {
    const func = block.func;
    const prog = func.program;

    const t = ast.getType(node).?;
    return switch (ast.get(node).*) {
        .unit => try block.op(t, .{
            .constant = try prog.constant(.unit),
        }),
        .bool => |v| try block.op(t, .{ .constant = try prog.constant(.{ .bool = v }) }),
        .int => |v| try block.op(t, .{ .constant = try prog.constant(.{ .uint = v }) }),
        .real => |v| try block.op(t, .{ .constant = try prog.constant(.{ .float = v }) }),

        .binary => |bin| bin: {
            const args = [2]ssa.Local{
                try lowerExpr(ast, block, bin.lhs),
                try lowerExpr(ast, block, bin.rhs),
            };

            const data: ssa.Opcode = switch (bin.op) {
                .add => .{ .add = args },
                .subtract => .{ .sub = args },
                .multiply => .{ .mul = args },
                .divide => .{ .div = args },
                .modulus => .{ .mod = args },

                else => @panic("TODO"),
            };

            break :bin try block.op(t, data);
        },

        .@"if" => |@"if"| try block.op(t, .{
            .branch = .{
                .cond = try lowerExpr(ast, block, @"if".cond),
                .if_true = try lowerBlockExpr(ast, func, @"if".if_true, null),
                .if_false = try lowerBlockExpr(ast, func, @"if".if_false, null),
            },
        }),

        else => @panic("TODO"),
    };
}

/// lower an expr to its own block
fn lowerBlockExpr(
    ast: *const Ast,
    func: *ssa.FuncBuilder,
    node: Ast.Node,
    phi: ?ssa.Local,
) Error!ssa.Block.Ref {
    var block = try func.block(phi);
    const ret = try lowerExpr(ast, &block, node);
    try block.build(ret);

    return block.ref;
}

fn lowerFunc(
    ast: *const Ast,
    prog: *ssa.Builder,
    node: Ast.Node,
    meta: Ast.Expr.Func,
) Error!ssa.Func.Ref {
    _ = node;

    var func = try prog.func();
    const entry = try lowerBlockExpr(ast, &func, meta.body, null);
    try func.build(entry);

    return func.ref;
}

fn lowerTopLevelExpr(
    ast: *const Ast,
    prog: *ssa.Builder,
    node: Ast.Node,
) Error!void {
    switch (ast.get(node).*) {
        .let => |let| {
            // TODO how should name bindings work?

            switch (ast.get(let.expr).*) {
                .func => |meta| {
                    _ = try lowerFunc(ast, prog, let.expr, meta);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

/// lower a program into ssa form
pub fn lower(
    ally: Allocator,
    ast: *const Ast,
    program_node: Ast.Node,
) Error!ssa.Program {
    var prog = ssa.Builder.init(ally);

    const expr = ast.get(program_node);
    std.debug.assert(expr.* == .program);

    for (expr.program) |toplevel| {
        try lowerTopLevelExpr(ast, &prog, toplevel);
    }

    return prog.build();
}
