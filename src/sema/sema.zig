const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const Value = fluent.Value;
const Name = fluent.Name;
const env = fluent.env;
const typer = fluent.typer;

pub const Error = Allocator.Error;

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

    const ExpectedMatching = struct {
        expected: Type.Id,
        found: Type.Id,
        expected_loc: Loc,
        found_loc: Loc,
    };

    expected: Expected,
    expected_matching: ExpectedMatching,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = ally;
        switch (self) {
            else => {},
        }
    }
};

/// generate an ast error and return error.InvalidType
fn invalidType(ast: *Ast, meta: SemaErrorMeta) SemaError {
    try ast.addError(.{ .semantic = meta });
    return SemaError.InvalidType;
}

/// check a node and expect it to match or subclass the expected type
fn expect(ast: *Ast, node: Ast.Node, expected: Type.Id) SemaError!void {
    const actual = ast.getType(node);
    if (!typer.isCompatible(actual, expected)) {
        return invalidType(ast, .{
            .expected = .{
                .loc = ast.getLoc(node),
                .expected = expected,
                .found = actual,
            },
        });
    }
}

/// check node and expects it to match the provided node
fn expectMatching(
    ast: *Ast,
    node: Ast.Node,
    to_match: Ast.Node,
) SemaError!void {
    const actual = ast.getType(node);
    const expected = ast.getType(to_match);
    if (!typer.isCompatible(actual, expected)) {
        return invalidType(ast, .{
            .expected_matching = .{
                .expected = expected,
                .found = actual,
                .expected_loc = ast.getLoc(to_match),
                .found_loc = ast.getLoc(node),
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
    _ = try ast.setType(node, typer.pre(.type));

    const type_ident = ast.get(node).ident;
    const type_name = try env.name(&.{type_ident});
    const type_value = env.lookup(type_name).?;
    return env.get(type_value).type;
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
            const lhs_type = try analyzeExpr(ast, meta.lhs, typer.pre(.number));
            _ = try expectMatching(ast, meta.rhs, meta.lhs);

            break :arith try ast.setType(node, lhs_type);
        },

        // conditions
        .eq => any: {
            _ = try analyzeExpr(ast, meta.lhs, typer.pre(.bool));
            _ = try analyzeExpr(ast, meta.rhs, typer.pre(.bool));
            break :any try ast.setType(node, typer.pre(.bool));
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
    @panic("TODO analyze call");
}

fn analyzeRecord(
    ast: *Ast,
    node: Ast.Node,
    entries: []const Ast.Expr.RecordEntry,
) SemaError!Type.Id {
    _ = ast;
    _ = node;
    _ = entries;
    @panic("TODO analyze records");
}

/// TODO this function will have to be redone from scratch once eval() exists,
/// so don't worry too much about making it correct
fn analyzeFn(
    ast: *Ast,
    node: Ast.Node,
    meta: Ast.Expr.Fn,
) SemaError!Type.Id {
    const ally = ast.ally;

    // name
    // TODO contextual namespace for this name
    const fn_name_ident = ast.get(meta.name).ident;
    const fn_name = try env.name(&.{fn_name_ident});

    // parameters
    var params = std.ArrayList(Type.Id).init(ally);
    defer params.deinit();
    var param_map = Value.FnDef.ParamMap{};
    errdefer param_map.deinit(ally);

    const record_entries = ast.get(meta.params).record;
    for (record_entries) |entry| {
        const param_type = try dirtyEvilHackAnalyzePredefType(ast, entry.value);
        try params.append(param_type);

        const pname_ident = ast.get(entry.key).ident;
        try param_map.put(ally, pname_ident, param_type);
    }

    const params_type = try typer.put(.{
        .@"struct" = .{ .fields = params.items },
    });

    _ = try ast.setType(meta.params, params_type);

    // collect fn type
    const return_type = try dirtyEvilHackAnalyzePredefType(ast, meta.returns);
    const body_type = try analyzeExpr(ast, meta.body, return_type);
    _ = body_type;

    const param_fields = typer.get(params_type).@"struct".fields;

    const func_type = try typer.put(.{
        .@"fn" = .{
            .params = param_fields,
            .returns = return_type,
        },
    });

    // define this function in the env
    _ = try env.put(fn_name, .{
        .fn_def = .{
            .type = func_type,
            .params = param_map,
            .ssa = .uncompiled,
        },
    });

    return try ast.setType(node, typer.pre(.unit));
}

/// dispatch for analysis
fn analyzeExpr(ast: *Ast, node: Ast.Node, expects: Type.Id) SemaError!Type.Id {
    const actual: Type.Id = switch (ast.get(node).*) {
        .unit => try ast.setType(node, typer.pre(.unit)),
        .bool => try ast.setType(node, typer.pre(.bool)),
        .number => number: {
            // TODO error for incompatible number, ensure output is a number,
            // etc. this just bypasses analysis
            // TODO I generate a typeclass based on the number's size, whether
            // it is negative, whether it contains fractions. this would not be
            // very hard and would lend to nice error output I think
            break :number try ast.setType(node, expects);
        },
        // TODO need two-stage compiler
        .ident => @panic("TODO analyze ident"),

        .unary => |meta| try analyzeUnary(ast, node, meta),
        .binary => |meta| try analyzeBinary(ast, node, meta),
        .record => |entries| try analyzeRecord(ast, node, entries),
        .call => |call| try analyzeCall(ast, node, call),
        .@"fn" => |meta| try analyzeFn(ast, node, meta),

        .@"if" => |@"if"| @"if": {
            _ = try analyzeExpr(ast, @"if".cond, typer.pre(.bool));
            const if_true_type = try analyzeExpr(ast, @"if".if_true, expects);
            const if_false_type = try analyzeExpr(ast, @"if".if_false, expects);
            const if_type = try typer.merge(&.{
                if_true_type,
                if_false_type,
            });

            break :@"if" try ast.setType(node, if_type);
        },

        .program => |prog| prog: {
            const prog_type = try ast.setType(node, typer.pre(.unit));

            for (prog) |child| {
                _ = try analyzeExpr(ast, child, typer.pre(.unit));
            }

            break :prog prog_type;
        },

        else => |expr| std.debug.panic("TODO analyze {s}", .{@tagName(expr)}),
    };

    try expect(ast, node, expects);

    return actual;
}

pub const Result = enum { ok, fail };

/// semantically analyze an expression
pub fn analyze(ast: *Ast, node: Ast.Node) Error!Result {
    if (analyzeExpr(ast, node, typer.pre(.any))) |_| {
        return .ok;
    } else |e| switch (e) {
        InvalidTypeError.InvalidType => {
            return .fail;
        },
        else => |filtered| {
            return @as(Error, @errSetCast(filtered));
        },
    }
}
