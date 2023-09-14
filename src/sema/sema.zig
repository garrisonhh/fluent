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
    Type.RenderError ||
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

/// dispatch for analysis
fn analyze(
    ast: *Ast,
    node: Ast.Node,
    expects: Type,
) Error!Type.Id {
    switch (ast.get(node).*) {
        .unit => {
            const unit = typer.predef(.unit);
            const resolved = try resolve(ast, ast.getLoc(node), expects, unit);
            try ast.setType(node, resolved);
        },

        else => |tag| std.debug.panic("TODO analyze {}", .{tag}),
    }
}

/// semantically analyze an expression
pub fn analyzeExpr(
    ast: *Ast,
    node: Ast.Node,
    expects: Type,
) Error!void {
    _ = try analyze(ast, node, expects);
}

/// semantically analyze a full ast
pub fn analyzeProgram(ally: Allocator, ast: *Ast) Error!void {
    try analyzeExpr(ally, ast, ast.root.?, typer.predef(.unit));
}
