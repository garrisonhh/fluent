//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");

pub const Node = com.Ref(.ast_node, 32);
const NodeMap = com.RefMap(Node, Expr);

pub const UnaryOp = enum {
    negate,
};

pub const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
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

    ident: []const u8,
    parens: Node,
    unary: Unary,
    binary: Binary,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .parens, .unary, .binary => {},
            .ident => |str| ally.free(str),
        }
    }
};

const Ast = @This();

map: NodeMap = .{},

pub fn deinit(self: *Ast, ally: Allocator) void {
    self.map.deinit(ally);
}

pub fn new(self: *Ast, ally: Allocator, expr: Expr) Allocator.Error!Node {
    return try self.map.put(ally, expr);
}

pub fn get(self: *const Ast, node: Node) *const Expr {
    return self.map.get(node);
}