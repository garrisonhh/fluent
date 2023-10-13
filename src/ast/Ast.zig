//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const blox = @import("blox");
const rendering = @import("rendering.zig");
const fluent = @import("../mod.zig");
const Loc = fluent.Loc;
const Type = fluent.Type;
const Value = fluent.Value;
const env = fluent.env;

pub const Error = @import("error.zig").Error;

pub const Node = com.Ref(.ast_node, 32);
const NodeMap = com.RefMap(Node, Expr);

pub const UnaryOp = enum {
    negate,
    addr,
};

pub const BinaryOp = enum {
    statement,
    field_access,

    add,
    subtract,
    multiply,
    divide,
    modulus,

    eq,
};

pub const Expr = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);

    pub const Unary = struct {
        op: UnaryOp,
        child: Node,
    };

    pub const Binary = struct {
        op: BinaryOp,
        lhs: Node,
        rhs: Node,
    };

    pub const RecordEntry = struct {
        key: Node,
        value: Node,
    };

    pub const Fn = struct {
        name: Node,
        params: Node,
        returns: Node,
        body: Node,
    };

    pub const If = struct {
        cond: Node,
        if_true: Node,
        if_false: Node,
    };

    value: Value.Ref,
    // TODO anyint + anyfloat with std.math.big numbers
    parens: Node,
    record: []const RecordEntry,
    call: []const Node,
    program: []const Node,
    unary: Unary,
    binary: Binary,
    @"fn": Fn,
    @"if": If,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .value,
            .parens,
            .unary,
            .binary,
            .@"fn",
            .@"if",
            => {},

            inline .record,
            .call,
            .program,
            => |slice| ally.free(slice),
        }
    }
};

const Ast = @This();

ally: Allocator,
errors: std.ArrayListUnmanaged(Error) = .{},
map: NodeMap = .{},

// node metadata
locs: std.AutoHashMapUnmanaged(Node, Loc) = .{},
types: std.AutoHashMapUnmanaged(Node, Type.Id) = .{},

pub fn init(ally: Allocator) Ast {
    return .{ .ally = ally };
}

pub fn deinit(self: *Ast) void {
    const ally = self.ally;

    var exprs = self.map.iterator();
    while (exprs.next()) |expr| expr.deinit(ally);
    self.map.deinit(ally);

    for (self.errors.items) |e| e.deinit(ally);
    self.errors.deinit(ally);

    self.locs.deinit(ally);
    self.types.deinit(ally);

    self.* = undefined;
}

pub const RenderError = rendering.RenderError;
pub const render = rendering.render;

/// add an error to the error list
pub fn addError(self: *Ast, e: Error) Allocator.Error!void {
    try self.errors.append(self.ally, e);
}

/// all of the stored errors
pub fn getErrors(self: *const Ast) []const Error {
    return self.errors.items;
}

pub fn new(self: *Ast, loc: Loc, expr: Expr) Allocator.Error!Node {
    const ally = self.ally;

    const node = try self.map.put(ally, expr);
    try self.locs.put(ally, node, loc);

    return node;
}

/// convenience function for creating a value expr
pub fn newValue(self: *Ast, loc: Loc, value: Value) Allocator.Error!Node {
    return try self.new(loc, .{
        .value = try env.value(self.ally, value),
    });
}

pub fn get(self: *const Ast, node: Node) *const Expr {
    return self.map.get(node);
}

pub fn getLoc(self: *const Ast, node: Node) Loc {
    return self.locs.get(node).?;
}

pub fn setType(self: *Ast, node: Node, t: Type.Id) Allocator.Error!Type.Id {
    const ally = self.ally;
    try self.types.put(ally, node, t);
    return t;
}

pub fn getType(self: *const Ast, node: Node) ?Type.Id {
    return self.types.get(node);
}

fn exprDataEql(self: *const Ast, a: anytype, b: @TypeOf(a)) bool {
    return switch (@TypeOf(a, b)) {
        void => true,

        bool, Expr.Int, Expr.Real => a == b,

        Node => self.eql(a, b),

        []const Node => arr: {
            if (a.len != b.len) {
                break :arr false;
            }

            for (a, b) |el_a, el_b| {
                if (!self.eql(el_a, el_b)) {
                    break :arr false;
                }
            }

            break :arr true;
        },

        Expr.Unary,
        Expr.Binary,
        Expr.Fn,
        Expr.If,
        => |T| st: {
            inline for (@typeInfo(T).Struct.fields) |field| {
                const are_eql = self.exprDataEql(
                    @field(a, field.name),
                    @field(b, field.name),
                );

                if (!are_eql) break :st false;
            }

            break :st true;
        },

        else => |T| {
            if (builtin.mode != .Debug) unreachable;
            std.debug.panic("TODO compare type {}", .{T});
        },
    };
}

/// check two nodes for structural equality
pub fn eql(self: *const Ast, a: Node, b: Node) bool {
    // nodes equal themselves
    if (a.eql(b)) return true;

    const fst = self.get(a);
    const snd = self.get(b);

    // tags must equal
    if (@as(Expr.Tag, fst.*) != @as(Expr.Tag, snd.*)) {
        return false;
    }

    // deeper inspection required
    switch (@as(Expr.Tag, fst.*)) {
        inline else => |tag| {
            const field_name = @tagName(tag);
            inline for (@typeInfo(Expr).Union.fields) |field_info| {
                if (comptime !std.mem.eql(u8, field_name, field_info.name)) {
                    continue;
                }

                const fst_data = @field(fst, field_name);
                const snd_data = @field(snd, field_name);

                return self.exprDataEql(fst_data, snd_data);
            } else unreachable;
        },
    }
}
