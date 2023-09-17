const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const typer = fluent.typer;

pub const Error =
    Allocator.Error ||
    typer.RenderError;

const InvalidTypeError = error{InvalidType};
const SemaError = Error || InvalidTypeError;

/// represents possible sema errors
pub const SemaErrorMeta = union(enum) {
    const Self = @This();

    const Expected = struct {
        loc: Loc,
        expected: Type.Id,
        found: Type.Id,
    };

    const ExpectedOneOf = struct {
        loc: Loc,
        expected: []const Type.Id,
        found: Type.Id,
    };

    const ExpectedMatching = struct {
        expected: Type.Id,
        found: Type.Id,
        expected_loc: Loc,
        found_loc: Loc,
    };

    expected: Expected,
    expected_one_of: ExpectedOneOf,
    expected_matching: ExpectedMatching,

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .expected_one_of => |exp| ally.free(exp.expected),
            else => {},
        }
    }
};

/// generate an ast error and return error.InvalidType
fn invalidType(ast: *Ast, meta: SemaErrorMeta) SemaError {
    try ast.addError(.{ .semantic = meta });
    return SemaError.InvalidType;
}

/// analyze a node and expect it to match the expected type
fn expect(ast: *Ast, node: Ast.Node, expected: Type.Id) SemaError!void {
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

/// expect with multiple options
fn expectOneOf(
    ast: *Ast,
    node: Ast.Node,
    expected: []const Type.Id,
) SemaError!Type.Id {
    const actual = try analyzeExpr(ast, node);
    for (expected) |t| {
        if (actual.eql(t)) {
            return actual;
        }
    } else {
        return invalidType(ast, .{
            .expected_one_of = .{
                .loc = ast.getLoc(node),
                .expected = try ast.ally.dupe(Type.Id, expected),
                .found = actual,
            },
        });
    }
}

fn expectMatching(
    ast: *Ast,
    node: Ast.Node,
    to_match: Ast.Node,
) SemaError!Type.Id {
    const actual = try analyzeExpr(ast, node);
    const expected = ast.getType(to_match).?;
    if (!actual.eql(expected)) {
        return invalidType(ast, .{
            .expected_matching = .{
                .expected = expected,
                .found = actual,
                .expected_loc = ast.getLoc(to_match),
                .found_loc = ast.getLoc(node),
            },
        });
    }

    return actual;
}

/// analyze a quoted node and expect it to match the expected type
fn expectQuoted(
    ast: *Ast,
    node: Ast.Node,
    expected: Type.Id,
) SemaError!void {
    const actual = try analyzeQuoted(ast, node);
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
) SemaError!Type.Id {
    _ = ast;
    _ = node;
    _ = meta;
    @panic("TODO analyze unary op");
}

fn analyzeBinary(
    ast: *Ast,
    node: Ast.Node,
    meta: Ast.Expr.Binary,
) SemaError!Type.Id {
    return switch (meta.op) {
        // arithmetic
        .add,
        .subtract,
        .multiply,
        .divide,
        .modulus,
        => arith: {
            const lhs_type = try expectOneOf(ast, meta.lhs, &.{
                typer.predef(.int),
                typer.predef(.float),
            });

            _ = try expectMatching(ast, meta.rhs, meta.lhs);

            break :arith try ast.setType(node, lhs_type);
        },

        else => |op| std.debug.panic(
            "TODO analyze binary {s}",
            .{@tagName(op)},
        ),
    };
}

fn analyzeFunc(ast: *Ast, node: Ast.Node, meta: Ast.Expr.Func,) SemaError!Type.Id {
    _ = node;

    const params = try analyzeExpr(ast, meta.params);
    const body = try analyzeExpr(ast, meta.body);

    // TODO function types
    _ = params;
    _ = body;

    return typer.predef(.unit);
}

fn analyzeQuoted(ast: *Ast, node: Ast.Node) SemaError!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.predef(.unit)),
        .bool => try ast.setType(node, typer.predef(.bool)),
        .ident => try ast.setType(node, typer.predef(.ident)),
        .int => try ast.setType(node, typer.predef(.int)),
        .real => try ast.setType(node, typer.predef(.float)),

        else => |expr| std.debug.panic(
            "TODO analyze quoted {s}",
            .{@tagName(expr)},
        ),
    };
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node) SemaError!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.predef(.unit)),
        .bool => try ast.setType(node, typer.predef(.bool)),
        .int => try ast.setType(node, typer.predef(.int)),
        .real => try ast.setType(node, typer.predef(.float)),

        .unary => |meta| try analyzeUnary(ast, node, meta),
        .binary => |meta| try analyzeBinary(ast, node, meta),
        .func => |meta| try analyzeFunc(ast, node, meta),

        .let => |let| let: {
            const let_type = try ast.setType(node, typer.predef(.unit));

            try expectQuoted(ast, let.name, typer.predef(.ident));
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

pub const Result = enum { ok, fail };

/// semantically analyze an expression
pub fn analyze(ast: *Ast, node: Ast.Node) Error!Result {
    return if (analyzeExpr(ast, node)) |_| .ok else |e| switch (e) {
        InvalidTypeError.InvalidType => {
            return .fail;
        },
        else => |filtered| {
            return @as(Error, @errSetCast(filtered));
        },
    };
}
