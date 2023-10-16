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

// TODO use typeclasses throughout sema for expectations

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

/// TODO insert implicit casts in expectations

/// analyze a node and expect it to match or subclass the expected type
fn expect(ast: *Ast, node: Ast.Node, expected: Type.Id) SemaError!Type.Id {
    const actual = try analyzeExpr(ast, node);
    if (!typer.isCompatible(actual, expected)) {
        return invalidType(ast, .{
            .expected = .{
                .loc = ast.getLoc(node),
                .expected = expected,
                .found = actual,
            },
        });
    }

    return actual;
}

// TODO `expectPeer` (peer resolution) instead of matching:
// 1. analyze all elements with an expectation
// 2. find the widest type (use std.sort.max with typer.isSubclass as `<`)
// 3. ensure all types are compatible with the widest type

/// analyzes node and expects it to match the provided node
fn expectMatching(
    ast: *Ast,
    node: Ast.Node,
    to_match: Ast.Node,
) SemaError!Type.Id {
    const actual = try analyzeExpr(ast, node);
    const expected = ast.getType(to_match).?;
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

    return actual;
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

    const value = ast.get(node).value;
    const type_name = env.get(value).name;
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
            const lhs_type = try expect(ast, meta.lhs, typer.pre(.number));
            _ = try expectMatching(ast, meta.rhs, meta.lhs);

            break :arith try ast.setType(node, lhs_type);
        },

        // conditions
        .eq => any: {
            _ = try analyzeExpr(ast, meta.lhs);
            _ = try expectMatching(ast, meta.rhs, meta.lhs);

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
    const fn_name_value = ast.get(meta.name).value;
    const fn_name = env.get(fn_name_value).name;

    // parameters
    var params = std.ArrayList(Type.Id).init(ally);
    defer params.deinit();
    var param_map = Value.FnDef.ParamMap{};
    errdefer param_map.deinit(ally);

    const record_entries = ast.get(meta.params).record;
    for (record_entries) |entry| {
        const param_type = try dirtyEvilHackAnalyzePredefType(ast, entry.value);
        try params.append(param_type);

        // ew
        const pname_value = ast.get(entry.key).value;
        const pname_name = env.get(pname_value).name;
        const pname_ident = env.nameSlice(pname_name)[0];
        try param_map.put(ally, pname_ident, param_type);
    }

    const params_type = try typer.put(.{
        .@"struct" = .{ .fields = params.items },
    });

    _ = try ast.setType(meta.params, params_type);

    // collect fn type
    const return_type = try dirtyEvilHackAnalyzePredefType(ast, meta.returns);
    const body_type = try analyzeExpr(ast, meta.body);

    _ = body_type; // TODO constrain this node with the return type

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
            .ssa = null,
        },
    });

    return try ast.setType(node, typer.pre(.unit));
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
            _ = try expect(ast, @"if".cond, typer.pre(.bool));
            _ = try analyzeExpr(ast, @"if".if_true);

            const branch_type = try expectMatching(
                ast,
                @"if".if_false,
                @"if".if_true,
            );

            break :@"if" try ast.setType(node, branch_type);
        },

        .program => |prog| prog: {
            const prog_type = try ast.setType(node, typer.pre(.unit));

            for (prog) |child| {
                _ = try expect(ast, child, typer.pre(.unit));
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
