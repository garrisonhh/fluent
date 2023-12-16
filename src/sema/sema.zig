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
const rendering = @import("rendering.zig");

// errors ======================================================================

/// represents possible sema errors
pub const TypeErrorMeta = union(enum) {
    const Self = @This();

    const UnknownName = struct {
        loc: Loc,
        name: Name,
        scope: Name,
    };

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

    unknown_name: UnknownName,
    expected: Expected,
    expected_matching: ExpectedMatching,

    pub const render = rendering.renderTypeError;
};

pub const TypeErrorBuf = fluent.ErrorBuf(TypeErrorMeta, error.InvalidType);
const TypeError = TypeErrorBuf.Error;

pub const Error = Allocator.Error || TypeError;

fn errorUnknownName(
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    loc: Loc,
    name: Name,
) TypeError {
    return ebuf.err(.{
        .unknown_name = .{
            .loc = loc,
            .name = name,
            .scope = st.unmanaged.scope,
        },
    });
}

/// check a node and expect it to match or subclass the expected type
fn expect(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    node: Ast.Node,
    expected: Type.Id,
) TypeError!void {
    const actual = ast.getType(node);
    if (!typer.isCompatible(actual, expected)) {
        return ebuf.err(.{
            .expected = .{
                .loc = ast.getLoc(node),
                .expected = expected,
                .found = actual,
            },
        });
    }
}

// analysis ====================================================================

const SymbolTable = fluent.ScopedMap(Type.Id);

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

fn analyzeUnary(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    meta: Ast.Expr.Unary,
    expects: Type.Id,
) Error!Type.Id {
    return switch (meta.op) {
        .negate => neg: {
            const t = try analyzeExpr(ast, ebuf, st, meta.child, expects);
            break :neg try ast.setType(node, t);
        },
        .addr => @panic("TODO analyze addr op"),
    };
}

fn analyzeBinary(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    meta: Ast.Expr.Binary,
    expects: Type.Id,
) Error!Type.Id {
    return switch (meta.op) {
        // arithmetic
        .add,
        .subtract,
        .multiply,
        .divide,
        .modulus,
        => arith: {
            const lhs_t = try analyzeExpr(ast, ebuf, st, meta.lhs, expects);
            const rhs_t = try analyzeExpr(ast, ebuf, st, meta.rhs, expects);
            const t = try typer.merge(&.{ lhs_t, rhs_t });
            break :arith try ast.setType(node, t);
        },

        // conditions
        .eq => any: {
            _ = try analyzeExpr(ast, ebuf, st, meta.lhs, typer.pre(.bool));
            _ = try analyzeExpr(ast, ebuf, st, meta.rhs, typer.pre(.bool));
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
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    call: []const Ast.Node,
) Error!Type.Id {
    _ = ast;
    _ = ebuf;
    _ = st;
    _ = node;
    _ = call;
    @panic("TODO analyze call");
}

fn analyzeRecord(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    entries: []const Ast.Expr.KV,
) Error!Type.Id {
    _ = ast;
    _ = ebuf;
    _ = st;
    _ = node;
    _ = entries;
    @panic("TODO analyze records");
}

/// TODO this function will have to be redone from scratch once eval() exists,
/// so don't worry too much about making it correct
fn analyzeFn(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    meta: Ast.Expr.Fn,
) Error!Type.Id {
    const ally = ast.ally;

    // name
    // TODO contextual namespace for this name
    const fn_name_ident = ast.get(meta.name).ident;
    const fn_name = try env.name(&.{fn_name_ident});

    // function scope
    try st.enterScope(fn_name_ident);
    defer st.exitScope();

    // analyze params
    var params = std.ArrayList(Type.Id).init(ally);
    defer params.deinit();
    var param_map = Value.FnDef.ParamMap{};
    errdefer param_map.deinit(env.ally);

    for (meta.params) |entry| {
        const param_type = try dirtyEvilHackAnalyzePredefType(ast, entry.value);
        try params.append(param_type);

        const pname_ident = ast.get(entry.key).ident;
        const pname = try env.name(&.{pname_ident});
        try param_map.put(env.ally, pname_ident, param_type);
        try st.put(pname, param_type);
    }

    // analyze return type and body
    const return_type = try dirtyEvilHackAnalyzePredefType(ast, meta.returns);
    _ = try analyzeExpr(ast, ebuf, st, meta.body, return_type);

    const func_type = try typer.put(.{
        .@"fn" = .{
            .params = params.items,
            .returns = return_type,
        },
    });

    // define this function in the env
    _ = try env.put(fn_name, .{
        .fn_def = .{
            .type = func_type,
            .params = param_map,
        },
    });

    return try ast.setType(node, typer.pre(.unit));
}

/// dispatch for analysis
/// TODO 'quote level' idea?
fn analyzeExpr(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    st: *SymbolTable,
    node: Ast.Node,
    expects: Type.Id,
) Error!Type.Id {
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
        // TODO need two-stage sema to predeclare func definitions
        .ident => |ident| ident: {
            const rel_name = try env.name(&.{ident});
            const t = t: {
                if (try st.get(rel_name)) |t| {
                    break :t t;
                } else if (env.lookupType(rel_name)) |t| {
                    break :t t;
                }

                return errorUnknownName(ebuf, st, ast.getLoc(node), rel_name);
            };

            break :ident try ast.setType(node, t);
        },

        .unary => |meta| try analyzeUnary(ast, ebuf, st, node, meta, expects),
        .binary => |meta| try analyzeBinary(ast, ebuf, st, node, meta, expects),
        .record => |entries| try analyzeRecord(ast, ebuf, st, node, entries),
        .call => |call| try analyzeCall(ast, ebuf, st, node, call),
        .@"fn" => |meta| try analyzeFn(ast, ebuf, st, node, meta),

        .@"if" => |@"if"| @"if": {
            _ = try analyzeExpr(ast, ebuf, st, @"if".cond, typer.pre(.bool));
            const if_true_t = try analyzeExpr(ast, ebuf, st, @"if".if_true, expects);
            const if_false_t = try analyzeExpr(ast, ebuf, st, @"if".if_false, expects);
            const if_type = try typer.merge(&.{
                if_true_t,
                if_false_t,
            });

            break :@"if" try ast.setType(node, if_type);
        },

        .program => |prog| prog: {
            const prog_type = try ast.setType(node, typer.pre(.unit));

            for (prog) |child| {
                _ = try analyzeExpr(ast, ebuf, st, child, typer.pre(.unit));
            }

            break :prog prog_type;
        },

        else => |expr| std.debug.panic("TODO analyze {s}", .{@tagName(expr)}),
    };

    try expect(ast, ebuf, node, expects);

    return actual;
}

/// semantically analyze an expression
pub fn analyze(
    ast: *Ast,
    ebuf: *TypeErrorBuf,
    scope: Name,
    node: Ast.Node,
) Error!void {
    var st = SymbolTable.init(ast.ally, scope);
    defer st.deinit();

    _ = try analyzeExpr(ast, ebuf, &st, node, typer.pre(.any));
}
