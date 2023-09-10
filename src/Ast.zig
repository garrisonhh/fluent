//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const blox = @import("blox");

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
        name: Node,
        expr: Node,
    };

    ident: []const u8,
    int: u64,
    real: f64,

    parens: Node,
    unary: Unary,
    binary: Binary,
    def: Def,
    program: []const Node,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .int,
            .real,
            .parens,
            .unary,
            .binary,
            .def,
            => {},

            inline .ident, .program => |slice| ally.free(slice),
        }
    }
};

const Ast = @This();

map: NodeMap = .{},
root: ?Node = null,

pub fn deinit(self: *Ast, ally: Allocator) void {
    var exprs = self.map.iterator();
    while (exprs.next()) |expr| expr.deinit(ally);

    self.map.deinit(ally);
    self.* = undefined;
}

pub fn new(self: *Ast, ally: Allocator, expr: Expr) Allocator.Error!Node {
    return try self.map.put(ally, expr);
}

pub fn get(self: *const Ast, node: Node) *const Expr {
    return self.map.get(node);
}

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

pub fn renderNode(
    self: *const Ast,
    mason: *blox.Mason,
    node: Node,
) RenderError!blox.Div {
    const indent = try mason.newSpacer(2, 0, .{});
    const space = try mason.newSpacer(1, 0, .{});

    const ally = mason.ally;
    const expr = self.get(node).*;
    return switch (expr) {
        .ident => |ident| try mason.newPre(ident, .{}),
        inline .int, .real => |n| n: {
            const text = try std.fmt.allocPrint(ally, "{d}", .{n});
            defer ally.free(text);

            break :n try mason.newPre(text, .{});
        },
        .def => |def| try mason.newBox(&.{
            try mason.newBox(&.{
                try mason.newPre("def", .{}),
                space,
                try self.renderNode(mason, def.name),
            }, .{ .direction = .right }),
            try mason.newBox(&.{
                indent,
                try self.renderNode(mason, def.expr),
            }, .{ .direction = .right }),
        }, .{}),
        .program => |prog| prog: {
            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            for (prog) |child| {
                try divs.append(try self.renderNode(mason, child));
            }

            break :prog try mason.newBox(divs.items, .{});
        },

        else => std.debug.panic("TODO render {}", .{@as(Expr.Tag, expr)}),
    };
}

pub fn render(self: *const Ast, mason: *blox.Mason) RenderError!blox.Div {
    if (self.root) |root| {
        return try self.renderNode(mason, root);
    }

    return try mason.newPre("<empty ast>", .{});
}
