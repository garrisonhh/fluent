const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Type = fluent.Type;
const typer = fluent.typer;

const InvalidType = error.InvalidType;
pub const SemaError = error{InvalidType};
pub const Error = Allocator.Error || SemaError;

pub fn analyzeExpr(
    ally: Allocator,
    ast: *Ast,
    node: Ast.Node,
    expects: Type.Id,
) Error!void {
    _ = ally;
    _ = ast;
    _ = node;
    _ = expects;
}

pub fn analyzeProgram(ally: Allocator, ast: *Ast) Error!void {
    try analyzeExpr(ally, ast, ast.root.?, typer.predef(.unit));
}
