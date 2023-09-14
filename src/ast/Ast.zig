//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const fluent = @import("../mod.zig");
const Loc = fluent.Loc;
const Type = fluent.Type;

pub const Node = com.Ref(.ast_node, 32);
const NodeMap = com.RefMap(Node, Expr);

pub const UnaryOp = enum {
    negate,
    addr,
};

pub const BinaryOp = enum {
    def,
    statement,
    field_access,

    add,
    subtract,
    multiply,
    divide,
    modulus,
};

pub const Expr = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);

    pub const Int = u64;
    pub const Real = f64;

    pub const Unary = struct {
        op: UnaryOp,
        child: Node,
    };

    pub const Binary = struct {
        op: BinaryOp,
        lhs: Node,
        rhs: Node,
    };

    pub const Let = struct {
        name: Node,
        expr: Node,
    };

    pub const If = struct {
        cond: Node,
        if_true: Node,
        if_false: Node,
    };

    unit,
    ident: []const u8,
    int: Int,
    real: Real,

    parens: Node,
    call: []const Node,
    program: []const Node,
    unary: Unary,
    binary: Binary,
    let: Let,
    @"if": If,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .unit,
            .int,
            .real,
            .parens,
            .unary,
            .binary,
            .let,
            .@"if",
            => {},

            inline .ident,
            .call,
            .program,
            => |slice| ally.free(slice),
        }
    }
};

const Ast = @This();

map: NodeMap = .{},
root: ?Node = null,

locs: std.AutoHashMapUnmanaged(Node, Loc) = .{},
types: std.AutoHashMapUnmanaged(Node, Type.Id) = .{},

pub fn deinit(self: *Ast, ally: Allocator) void {
    var exprs = self.map.iterator();
    while (exprs.next()) |expr| expr.deinit(ally);

    self.map.deinit(ally);
    self.locs.deinit(ally);
    self.types.deinit(ally);

    self.* = undefined;
}

pub const RenderError = rendering.RenderError;
pub const renderNode = rendering.renderNode;
pub const render = rendering.render;

pub fn new(
    self: *Ast,
    ally: Allocator,
    loc: ?Loc,
    expr: Expr,
) Allocator.Error!Node {
    const node = try self.map.put(ally, expr);
    if (loc) |x| {
        try self.locs.putNoClobber(ally, node, x);
    }

    return node;
}

pub fn get(self: *const Ast, node: Node) *const Expr {
    return self.map.get(node);
}

pub fn getLoc(self: *const Ast, node: Node) ?Loc {
    return self.locs.get(node);
}

pub fn setType(
    self: *Ast,
    ally: Allocator,
    node: Node,
    t: Type.Id,
) Allocator.Error!void {
    try self.types.put(ally, node, t);
}

pub fn getType(self: *const Ast, node: Node) ?Type.Id {
    return self.types.get(node);
}

fn exprDataEql(self: *const Ast, a: anytype, b: @TypeOf(a)) bool {
    return switch (@TypeOf(a, b)) {
        void => true,
        Expr.Int, Expr.Real => a == b,
        []const u8 => std.mem.eql(u8, a, b),
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
