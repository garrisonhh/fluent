const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const typer = fluent.typer;

pub const SemaError = error{InvalidType};
pub const Error =
    Allocator.Error ||
    typer.RenderError ||
    SemaError;

/// represents possible sema errors
pub const SemaErrorMeta = union(enum) {
    const Expected = struct {
        loc: Loc,
        expected: Type.Id,
        found: Type.Id,
    };

    expected: Expected,
};

/// generate an ast error and return error.InvalidType
fn invalidType(ast: *Ast, meta: SemaErrorMeta) Error {
    try ast.addError(.{ .semantic = meta });
    return Error.InvalidType;
}

/// analyze a node and expect the type of this node to match the expected type
fn expect(ast: *Ast, node: Ast.Node, expected: Type.Id) Error!void {
    const actual = try analyzeExpr(ast, node);
    if (!actual.eql(expected)) {
        return invalidType(ast, .{
            .expected = .{
                .loc = ast.getLoc(node),
                .expected = expected,
                .found = actual,
            },
        });
    }
}

fn analyzeUnary(
    ast: *Ast,
    node: Ast.Node,
    meta: Ast.Expr.Unary,
) Error!Type.Id {
    _ = ast;
    _ = node;
    _ = meta;
    @panic("TODO analyze unary op");
}


fn analyzeBinary(
    ast: *Ast,
    node: Ast.Node,
    meta: Ast.Expr.Binary,
) Error!Type.Id {
    _ = ast;
    _ = node;
    _ = meta;
    @panic("TODO analyze binary op");
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node) Error!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.predef(.unit)),
        .ident => try ast.setType(node, typer.predef(.ident)),
        .bool => try ast.setType(node, typer.predef(.bool)),
        .int => try ast.setType(node, typer.predef(.int)),
        .real => try ast.setType(node, typer.predef(.float)),

        .unary => |meta| try analyzeUnary(ast, node, meta),
        .binary => |meta| try analyzeBinary(ast, node, meta),

        .let => |let| let: {
            const let_type = try ast.setType(node, typer.predef(.unit));

            try expect(ast, let.name, typer.predef(.ident));
            _ = try analyzeExpr(ast, let.expr);

            break :let let_type;
        },

        .program => |prog| prog: {
            const prog_type = try ast.setType(node, typer.predef(.unit));

            for (prog) |child| {
                _ = try analyzeExpr(ast, child);
            }

            break :prog prog_type;
        },

        else => |expr| std.debug.panic("TODO analyze {s}", .{@tagName(expr)}),
    };
}

/// semantically analyze an expression
pub fn analyze(ast: *Ast, node: Ast.Node) Error!void {
    _ = try analyzeExpr(ast, node);
}
