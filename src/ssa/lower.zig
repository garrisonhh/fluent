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

fn lowerFunction(
    ast: *const Ast,
    builder: *ssa.Builder,
    name_node: ?Ast.Node,
    params: Ast.Node,
    body: Ast.Node,
) Error!ssa.Func.Ref {
    // TODO this is a dirty hack
    const name: ?Name = if (name_node) |nnode| name: {
        const name_ident = ast.get(nnode).ident;
        break :name try env.name(&.{name_ident});
    } else null;

    var func = try builder.func(name);

    // add params
    const params_type = ast.getType(params);
    const param_types = typer.get(params_type).@"struct".fields;
    for (param_types) |param_type| {
        _ = try func.param(param_type);
    }

    // compile body
    var block = try func.block(null);
    func.entry = block.ref;

    const val = try lowerExpr(ast, &block, body);
    try block.branch(.{ .ret = val });

    func.exit = block.ref;

    return func.ref;
}

fn lowerExpr(
    ast: *const Ast,
    block: **ssa.BlockBuilder,
    node: Ast.Node,
) Error!ssa.Local {
    const t = ast.getType(node);
    return switch (ast.get(node).*) {
        .unit => try lowerConstant(block.*, t, .unit),
        .bool => |b| try lowerConstant(block.*, t, .{ .bool = b }),
        .number => |n| try lowerNumber(block.*, t, n),
        .ident => @panic("TODO lower ident"),

        .binary => |bin| bin: {
            const args = [2]ssa.Local{
                try lowerExpr(ast, block, bin.lhs),
                try lowerExpr(ast, block, bin.rhs),
            };

            const inst: ssa.Instruction = switch (bin.op) {
                .add => .{ .add = args },
                .subtract => .{ .sub = args },
                .multiply => .{ .mul = args },
                .divide => .{ .div = args },
                .modulus => .{ .mod = args },

                else => @panic("TODO"),
            };

            break :bin try block.*.op(t, inst);
        },

        .@"if" => |@"if"| @"if": {
            const func = block.*.func;
            const if_true = try func.block(null);
            const if_false = try func.block(null);
            const next = try func.block(t);

            // lower cond + branch op
            const cond = try lowerExpr(ast, block, @"if".cond);
            try block.*.branch(.{
                .branch = .{
                    .cond = cond,
                    .if_true = if_true.ref,
                    .if_false = if_false.ref,
                },
            });

            // lower branches
            block.* = if_true;
            const if_true_val = try lowerExpr(ast, block, @"if".if_true);
            try block.*.branch(.{
                .jump = .{ .phi = if_true_val, .to = next.ref },
            });

            block.* = if_false;
            const if_false_val = try lowerExpr(ast, block, @"if".if_false);
            try block.*.branch(.{
                .jump = .{ .phi = if_false_val, .to = next.ref },
            });

            // switch to next block
            block.* = next;

            break :@"if" next.phi.?;
        },

        else => |tag| {
            std.debug.panic("TODO lower {s} exprs", .{@tagName(tag)});
        },
    };
}

/// lower an analyzed ast onto the ssa program context, and returns a func ref
/// to the entry point of the program or expression you are lowering.
pub fn lower(
    ally: Allocator,
    ast: *const Ast,
    prog_node: Ast.Node,
) Error!ssa.Object {
    const prog_expr = ast.get(prog_node);
    std.debug.assert(prog_expr.* == .program);

    var builder = ssa.Builder.init(ally);
    errdefer builder.errdeinit();

    for (prog_expr.program) |decl| {
        switch (ast.get(decl).*) {
            .@"fn" => |@"fn"| {
                _ = try lowerFunction(
                    ast,
                    &builder,
                    @"fn".name,
                    @"fn".params,
                    @"fn".body,
                );
            },

            else => |tag| {
                std.debug.panic("TODO lower {s} decls", .{@tagName(tag)});
            },
        }
    }

    return try builder.build();
}
