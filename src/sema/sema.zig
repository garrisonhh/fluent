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

/// generate an ast error and return error.InvalidType
fn invalidType(ast: *Ast, loc: ?Loc, desc: blox.Div) Error {
    try ast.addError(loc, desc);
    return Error.InvalidType;
}

fn errorExpected(
    ast: *Ast,
    loc: ?Loc,
    expected: Type.Id,
    found: Type.Id,
) Error {
    const mason = &ast.error_mason;
    const desc = try mason.newBox(&.{
        try mason.newPre("expected ", .{}),
        try typer.render(mason, expected),
        try mason.newPre(", found ", .{}),
        try typer.render(mason, found),
    }, .{ .direction = .right });

    return invalidType(ast, loc, desc);
}

/// analyze a node and expect the type of this node to match the expected type
fn expect(ast: *Ast, node: Ast.Node, expected: Type.Id) Error!void {
    const actual = try analyzeExpr(ast, node);
    if (!actual.eql(expected)) {
        return errorExpected(ast, ast.getLoc(node), expected, actual);
    }
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node) Error!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.predef(.unit)),
        .ident => try ast.setType(node, typer.predef(.ident)),
        .bool => try ast.setType(node, typer.predef(.bool)),
        .int => try ast.setType(node, typer.predef(.int)),
        .real => try ast.setType(node, typer.predef(.float)),

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

        else => |tag| std.debug.panic("TODO analyze {}", .{tag}),
    };
}

/// semantically analyze an expression
pub fn analyze(ast: *Ast, node: Ast.Node) Error!void {
    _ = try analyzeExpr(ast, node);
}
