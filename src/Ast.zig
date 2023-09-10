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

    pub const Def = struct {
        ident: []const u8,
        expr: Node,
    };

    ident: []const u8,
    parens: Node,
    unary: Unary,
    binary: Binary,
    def: Def,
    program: []const Node,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .parens, .unary, .binary => {},

            inline .ident, .program => |slice| ally.free(slice),

            .def => |def| ally.free(def.ident),
        }
    }
};

const Ast = @This();

map: NodeMap = .{},
root: ?Node = null,

pub fn deinit(self: *Ast, ally: Allocator) void {
    self.root = null;
    self.map.deinit(ally);
}

pub fn new(self: *Ast, ally: Allocator, expr: Expr) Allocator.Error!Node {
    return try self.map.put(ally, expr);
}

pub fn get(self: *const Ast, node: Node) *const Expr {
    return self.map.get(node);
}