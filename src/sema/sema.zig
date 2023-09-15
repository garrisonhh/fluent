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
    expects: Type.Id,
    actual: Type.Id,
) Error {
    const mason = &ast.error_mason;
    const desc = try mason.newBox(&.{
        try mason.newPre("expected ", .{}),
        try typer.render(mason, expects),
        try mason.newPre(", found ", .{}),
        try typer.render(mason, actual),
    }, .{ .direction = .left });

    return invalidType(ast, loc, desc);
}

/// resolves a basic outer/inner type expectation
fn resolve(
    ast: *Ast,
    loc: ?Loc,
    expected: Type.Id,
    actual: Type.Id,
) Error!Type.Id {
    const exp = typer.get(expected);
    return switch (exp.*) {
        .any => actual,

        // exact match expected
        .unit,
        .bool,
        .int,
        .float,
        => exact: {
            if (!expected.eql(actual)) {
                break :exact errorExpected(ast, loc, expected, actual);
            }

            break :exact expected;
        },
    };
}

/// resolves and sets the type fo the node
fn expect(
    ast: *Ast,
    node: Ast.Node,
    expects: Type.Id,
    actual: Type.Id,
) Error!Type.Id {
    const res = try resolve(ast, ast.getLoc(node), expects, actual);
    try ast.setType(node, res);
    return res;
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node, expects: Type.Id) Error!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try expect(ast, node, expects, typer.predef(.unit)),
        .bool => try expect(ast, node, expects, typer.predef(.bool)),
        .int => try expect(ast, node, expects, typer.predef(.int)),
        .real => try expect(ast, node, expects, typer.predef(.float)),

        .let => |let| let: {
            const unit = typer.predef(.unit);
            const any = typer.predef(.any);

            // let is unit
            const let_res = try resolve(ast, ast.getLoc(node), expects, unit);
            try ast.setType(node, let_res);

            // TODO analyze let.name as quoted ident

            // recurse on expr
            // TODO add type annotation to let expr
            _ = try analyzeExpr(ast, let.expr, any);

            break :let let_res;
        },

        .program => |prog| prog: {
            const unit = typer.predef(.unit);
            const any = typer.predef(.any);

            // prog is unit
            const prog_res = try resolve(ast, ast.getLoc(node), expects, unit);
            try ast.setType(node, prog_res);

            // children are any
            for (prog) |child| {
                _ = try analyzeExpr(ast, child, any);
            }

            break :prog prog_res;
        },

        else => |tag| std.debug.panic("TODO analyze {}", .{tag}),
    };
}

/// semantically analyze an expression
pub fn analyze(ast: *Ast, node: Ast.Node) Error!void {
    _ = try analyzeExpr(ast, node, typer.predef(.any));
}
