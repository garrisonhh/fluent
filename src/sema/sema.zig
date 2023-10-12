const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const Value = fluent.Value;
const env = fluent.env;
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

/// analyzes node and expects it to match the provided node
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

/// TODO remove this and do it correctly once I can eval types
fn dirtyEvilHackAnalyzePredefType(
    ast: *Ast,
    node: Ast.Node,
) Allocator.Error!Type.Id {
    _ = try ast.setType(node, typer.predef(.type));

    const value = ast.get(node).value;
    const type_name = env.get(value).name;
    const type_value = env.lookup(type_name).?;
    return env.get(type_value).type;
}

/// dirty evil hack to get function parameter sema working
/// TODO remove this and do it correctly once I can eval types
fn dirtyEvilHackAnalyzeFuncParams(ast: *Ast, node: Ast.Node) SemaError!Type.Id {
    const ally = ast.ally;

    var params = std.ArrayList(Type.Id).init(ally);
    defer params.deinit();

    const record_entries = ast.get(node).record;
    for (record_entries) |entry| {
        // TODO store func param names in env
        // const name = ast.get(entry.key).ident;

        const param = try dirtyEvilHackAnalyzePredefType(ast, entry.value);
        try params.append(param);
    }

    const params_type = try typer.put(ally, .{
        .@"struct" = .{ .fields = try params.toOwnedSlice() },
    });

    return try ast.setType(node, params_type);
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
            const lhs_type = try expectOneOf(
                ast,
                meta.lhs,
                typer.predefClass(.numbers),
            );

            _ = try expectMatching(ast, meta.rhs, meta.lhs);

            break :arith try ast.setType(node, lhs_type);
        },

        // conditions
        .eq => any: {
            _ = try analyzeExpr(ast, meta.lhs);
            _ = try expectMatching(ast, meta.rhs, meta.lhs);

            break :any try ast.setType(node, typer.predef(.bool));
        },

        else => |op| std.debug.panic(
            "TODO analyze binary {s}",
            .{@tagName(op)},
        ),
    };
}

fn analyzeCall(
    ast: *Ast,
    node: Ast.Node,
    call: []const Ast.Node,
) SemaError!Type.Id {
    _ = ast;
    _ = node;
    _ = call;

    // TODO analyse call

    return typer.predef(.unit);
}

fn analyzeRecord(
    ast: *Ast,
    node: Ast.Node,
    entries: []const Ast.Expr.RecordEntry,
) SemaError!Type.Id {
    _ = ast;
    _ = node;
    _ = entries;

    // TODO analyze records
    return typer.predef(.unit);
}

fn analyzeFn(
    ast: *Ast,
    node: Ast.Node,
    meta: Ast.Expr.Fn,
) SemaError!Type.Id {
    const ally = ast.ally;

    const params_type = try dirtyEvilHackAnalyzeFuncParams(ast, meta.params);
    const return_type = try dirtyEvilHackAnalyzePredefType(ast, meta.returns);
    const body_type = try analyzeExpr(ast, meta.body);

    _ = body_type; // TODO constrain this node with the return type

    // construct fn type
    const param_fields = typer.get(params_type).@"struct".fields;

    const func_type = try typer.put(ally, .{
        .@"fn" = .{
            .params = try ally.dupe(Type.Id, param_fields),
            .returns = return_type,
        },
    });

    return try ast.setType(node, func_type);
}

fn analyzeQuoted(ast: *Ast, node: Ast.Node) SemaError!Type.Id {
    return switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.predef(.unit)),
        .bool => try ast.setType(node, typer.predef(.bool)),
        .ident => try ast.setType(node, typer.predef(.ident)),
        .int => try ast.setType(node, typer.predef(.i64)),
        .real => try ast.setType(node, typer.predef(.f64)),

        else => |expr| std.debug.panic(
            "TODO analyze quoted {s}",
            .{@tagName(expr)},
        ),
    };
}

fn analyzeValue(ast: *Ast, node: Ast.Node, ref: Value.Ref) SemaError!Type.Id {
    const t = env.get(ref).findType();
    return try ast.setType(node, t);
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node) SemaError!Type.Id {
    return switch (ast.get(node).*) {
        .value => |ref| try analyzeValue(ast, node, ref),
        .unary => |meta| try analyzeUnary(ast, node, meta),
        .binary => |meta| try analyzeBinary(ast, node, meta),
        .record => |entries| try analyzeRecord(ast, node, entries),
        .call => |call| try analyzeCall(ast, node, call),
        .@"fn" => |meta| try analyzeFn(ast, node, meta),

        .@"if" => |@"if"| @"if": {
            _ = try expect(ast, @"if".cond, typer.predef(.bool));
            _ = try analyzeExpr(ast, @"if".if_true);

            const branch_type = try expectMatching(
                ast,
                @"if".if_false,
                @"if".if_true,
            );

            break :@"if" try ast.setType(node, branch_type);
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
