const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Name = fluent.Name;
const Type = fluent.Type;
const Value = fluent.Value;
const env = fluent.env;
const ssa = fluent.ssa;
const typer = fluent.typer;

pub const Error = Allocator.Error || Ast.Expr.Number.ParseError;

/// sugar for creating a value ref and lowering it as a constant op
fn lowerConstant(
    block: *ssa.BlockBuilder,
    t: Type.Id,
    init_value: Value,
) Error!ssa.Local {
    return try block.op(t, .{
        .constant = try env.value(init_value),
    });
}

fn lowerNumber(
    block: *ssa.BlockBuilder,
    t: Type.Id,
    num: Ast.Expr.Number,
) Error!ssa.Local {
    const init_value: Value = switch (typer.get(t).*) {
        .int => |int| switch (int.signedness) {
            inline else => |signedness| switch (int.bits) {
                inline else => |bits| int: {
                    const T = std.meta.Int(signedness, bits.count());
                    const n = try num.parseInto(T);

                    break :int switch (signedness) {
                        .unsigned => .{
                            .uint = @unionInit(Value.UInt, @typeName(T), n),
                        },
                        .signed => .{
                            .int = @unionInit(Value.Int, @typeName(T), n),
                        },
                    };
                },
            },
        },
        .float => |float| switch (float.bits) {
            inline else => |bits| float: {
                const F = std.meta.Float(bits.count());
                const n = try num.parseInto(F);

                break :float .{
                    .float = @unionInit(Value.Float, @typeName(F), n),
                };
            },
        },
        else => @panic("unknown number type"),
    };

    return try lowerConstant(block, t, init_value);
}

/// lower an expr to its own block
fn lowerBlockExpr(
    ast: *const Ast,
    prog: *ssa.Program,
    func: *ssa.FuncBuilder,
    node: Ast.Node,
) Error!ssa.Block.Ref {
    var block = try func.block();
    const ret = try lowerExpr(ast, prog, &block, node);
    try block.build(ret);

    return block.ref;
}

fn lowerFunction(
    ast: *const Ast,
    prog: *ssa.Program,
    name: Name,
    params: Ast.Node,
    body: Ast.Node,
) Error!ssa.Func.Ref {
    var func = try prog.func(name);

    // compile entry block
    const params_type = ast.getTypeOpt(params).?;
    const param_types = typer.get(params_type).@"struct".fields;
    for (param_types) |param_type| {
        _ = try func.param(param_type);
    }

    const entry = try lowerBlockExpr(ast, prog, &func, body);

    // build + update env entry
    try func.build(entry);

    const func_value_ref = env.lookup(name).?;
    const func_value = env.getMut(func_value_ref);
    std.debug.assert(func_value.fn_def.ssa == .uncompiled);
    func_value.fn_def.ssa = .{ .compiled = func.ref };

    return func.ref;
}

fn lowerExpr(
    ast: *const Ast,
    prog: *ssa.Program,
    block: *ssa.BlockBuilder,
    node: Ast.Node,
) Error!ssa.Local {
    const func = block.func;

    const t = ast.getType(node);
    return switch (ast.get(node).*) {
        .unit => try lowerConstant(block, t, .unit),
        .bool => |b| try lowerConstant(block, t, .{ .bool = b }),
        .number => |n| try lowerNumber(block, t, n),
        .ident => @panic("TODO lower ident"),

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
            const if_true = try lowerBlockExpr(ast, prog, func, @"if".if_true);
            const if_false =
                try lowerBlockExpr(ast, prog, func, @"if".if_false);

            break :@"if" try block.op(t, .{
                .branch = .{
                    .cond = cond,
                    .if_true = if_true,
                    .if_false = if_false,
                },
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
    prog_node: Ast.Node,
) Error!void {
    const prog_expr = ast.get(prog_node);
    std.debug.assert(prog_expr.* == .program);

    for (prog_expr.program) |decl| {
        switch (ast.get(decl).*) {
            .@"fn" => |@"fn"| {
                // TODO eval this; this is a dirty evil hack
                const fn_name_ident = ast.get(@"fn".name).ident;
                const fn_name = try env.name(&.{fn_name_ident});

                _ = try lowerFunction(
                    ast,
                    program,
                    fn_name,
                    @"fn".params,
                    @"fn".body,
                );
            },

            else => |tag| {
                std.debug.panic("TODO lower {s} decls", .{@tagName(tag)});
            },
        }
    }
}
