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

// rendering ===================================================================

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const theme = struct {
    const c = blox.Color.init;

    const tag = c(.bright, .cyan);
    const data = c(.bright, .magenta);
    const field = c(.normal, .yellow);
    const ident = c(.bright, .red);
};

fn renderFieldData(
    self: *const Ast,
    mason: *blox.Mason,
    comptime T: type,
    value: T,
) RenderError!blox.Div {
    const ally = mason.ally;
    return switch (T) {
        // enums
        UnaryOp,
        BinaryOp,
        => try mason.newPre(@tagName(value), .{ .fg = theme.data }),

        // child nodes
        Node => try self.renderNode(mason, value),
        []const Node => nodes: {
            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            if (value.len == 0) {
                try divs.append(try mason.newPre("{}", .{}));
            } else {
                for (value, 0..) |node, i| {
                    const leader = if (i == 0) "{ " else ", ";
                    try divs.append(try mason.newBox(&.{
                        try mason.newPre(leader, .{}),
                        try self.renderNode(mason, node),
                    }, .{ .direction = .right }));
                }

                try divs.append(try mason.newPre("}", .{}));
            }

            break :nodes try mason.newBox(divs.items, .{});
        },

        else => unreachable,
    };
}

pub fn renderNode(
    self: *const Ast,
    mason: *blox.Mason,
    node: Node,
) RenderError!blox.Div {
    const indent = try mason.newSpacer(2, 0, .{});
    const space = try mason.newSpacer(1, 0, .{});

    // rendering
    const ally = mason.ally;
    const expr = self.get(node).*;

    const tag = try mason.newPre(@tagName(expr), .{ .fg = theme.tag });

    return switch (expr) {
        // literals
        .ident => |ident| try mason.newBox(&.{
            tag,
            space,
            try mason.newPre(ident, .{ .fg = theme.ident }),
        }, .{ .direction = .right }),

        inline .int, .real => |n| n: {
            const text = try std.fmt.allocPrint(ally, "{d}", .{n});
            defer ally.free(text);

            break :n try mason.newBox(&.{
                tag,
                space,
                try mason.newPre(text, .{ .fg = theme.data }),
            }, .{ .direction = .right });
        },

        // containers
        inline .parens,
        .unary,
        .binary,
        .def,
        .program,
        => |data| div: {
            const Data = @TypeOf(data);
            const info = @typeInfo(Data);

            const data_div = if (comptime info == .Struct) data: {
                var fields = std.ArrayList(blox.Div).init(ally);
                defer fields.deinit();

                inline for (info.Struct.fields) |field| {
                    const field_data = try self.renderFieldData(
                        mason,
                        field.type,
                        @field(data, field.name),
                    );

                    try fields.append(try mason.newBox(&.{
                        try mason.newPre(field.name, .{ .fg = theme.field }),
                        try mason.newPre(": ", .{}),
                        field_data,
                    }, .{ .direction = .right }));
                }

                break :data try mason.newBox(fields.items, .{});
            } else try self.renderFieldData(mason, Data, data);

            break :div try mason.newBox(&.{
                tag,
                try mason.newBox(&.{
                    indent,
                    data_div,
                }, .{ .direction = .right }),
            }, .{});
        },
    };
}

pub fn render(self: *const Ast, mason: *blox.Mason) RenderError!blox.Div {
    if (self.root) |root| {
        return try self.renderNode(mason, root);
    }

    return try mason.newPre("<empty ast>", .{});
}
