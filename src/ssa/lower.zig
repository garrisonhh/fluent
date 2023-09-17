const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const ssa = fluent.ssa;

pub const Error = Allocator.Error;

fn lowerValue(
    ast: *const Ast,
    b: *ssa.Builder,
    node: Ast.Node,
) Error!void {
    _ = ast;
    _ = b;
    _ = node;
    @panic("TODO");
}

fn lowerTopLevelExpr(
    ast: *const Ast,
    b: *ssa.Builder,
    node: Ast.Node,
) Error!void {
    switch (ast.get(node).*) {
        .let => |let| {
            // TODO how should name bindings work?
            try lowerValue(ast, b, let.expr);
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
    var b = ssa.Builder.init(ally);

    const expr = ast.get(program_node);
    std.debug.assert(expr.* == .program);

    for (expr.program) |toplevel| {
        try lowerTopLevelExpr(ast, &b, toplevel);
    }

    return b.build();
}
