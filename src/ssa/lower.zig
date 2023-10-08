const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const ssa = fluent.ssa;
const typer = fluent.typer;

pub const Error = Allocator.Error;

fn lowerTopLevelExpr(
    ast: *const Ast,
    prog: *ssa.Program,
    node: Ast.Node,
) Error!void {
    switch (ast.get(node).*) {
        .let => |let| {
            // TODO how should name bindings work?

            // TODO execute the body as code?

            // functions
            const expr = ast.get(let.expr);
            if (expr.* == .binary and expr.binary.op == .function) {
                _ = try lowerFunction(
                    ast,
                    prog,
                    expr.binary.lhs,
                    expr.binary.rhs,
                );
            } else {
                @panic("TODO execute let const");
            }
        },
        else => unreachable,
    }
}

/// lower an expr to its own block
fn lowerBlockExpr(
    ast: *const Ast,
    prog: *ssa.Program,
    func: *ssa.FuncBuilder,
    node: Ast.Node,
    phi: ?ssa.Local,
) Error!ssa.Block.Ref {
    var block = try func.block(phi);
    const ret = try lowerExpr(ast, prog, &block, node);
    try block.build(ret);

    return block.ref;
}

fn lowerFunction(
    ast: *const Ast,
    prog: *ssa.Program,
    params: Ast.Node,
    body: Ast.Node,
) Error!ssa.Func.Ref {
    var func = try prog.func();

    const params_type = ast.getType(params).?;
    const param_types = typer.get(params_type).@"struct".fields;
    for (param_types) |param_type| {
        _ = try func.param(param_type);
    }

    const entry = try lowerBlockExpr(ast, prog, &func, body, null);
    try func.build(entry);

    return func.ref;
}

fn lowerExpr(
    ast: *const Ast,
    prog: *ssa.Program,
    block: *ssa.BlockBuilder,
    node: Ast.Node,
) Error!ssa.Local {
    const func = block.func;

    const t = ast.getType(node).?;
    return switch (ast.get(node).*) {
        .unit => try block.op(t, .{
            .constant = try func.constant(.unit),
        }),
        .bool => |v| try block.op(t, .{
            .constant = try func.constant(.{ .bool = v }),
        }),
        .int => |v| try block.op(t, .{
            .constant = try func.constant(.{ .uint = v }),
        }),
        .real => |v| try block.op(t, .{
            .constant = try func.constant(.{ .float = v }),
        }),

        .binary => |bin| bin: {
            const args = [2]ssa.Local{
                try lowerExpr(ast, prog, block, bin.lhs),
                try lowerExpr(ast, prog, block, bin.rhs),
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

        .@"if" => |@"if"| @"if": {
            const cond = try lowerExpr(ast, prog, block, @"if".cond);
            const if_true =
                try lowerBlockExpr(ast, prog, func, @"if".if_true, cond);
            const if_false =
                try lowerBlockExpr(ast, prog, func, @"if".if_false, cond);

            break :@"if" try block.op(t, .{
                .branch = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                },
            });
        },
        .@"fn" => |@"fn"| @"fn": {
            const func_ref =
                try lowerFunction(ast, prog, @"fn".params, @"fn".body);

            break :@"fn" try block.op(t, .{
                .constant = try func.constant(.{ .func_ref = func_ref }),
            });
        },

        else => |tag| {
            std.debug.panic("TODO lower {s} exprs", .{@tagName(tag)});
        },
    };
}

/// lower an analyzed ast onto the ssa program context, and returns a func ref
/// to the entry point of the program or expression you are lowering.
pub fn lower(
    ast: *const Ast,
    program: *ssa.Program,
    node: Ast.Node,
) Error!ssa.Func.Ref {
    var func = try program.func();

    const entry = switch (ast.get(node).*) {
        .program => @panic("TODO lower program"),
        else => try lowerBlockExpr(ast, program, &func, node, null),
    };

    try func.build(entry);
    return func.ref;
}
