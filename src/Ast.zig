//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const blox = @import("blox");
const literals = @import("parser/literals.zig");

pub const Node = com.Ref(.ast_node, 32);
const NodeMap = com.RefMap(Node, Expr);

pub const UnaryOp = enum {
    negate,
    addr,
};

pub const BinaryOp = enum {
    statement,
    ns_access,
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

    pub const Int = literals.Int;
    pub const Real = literals.Real;

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

    unit,
    ident: []const u8,
    int: Int,
    real: Real,

    parens: Node,
    call: []const Node,
    unary: Unary,
    binary: Binary,
    def: Def,
    program: []const Node,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .unit,
            .int,
            .real,
            .parens,
            .unary,
            .binary,
            .def,
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
        Expr.Def,
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
        }
    }
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
        void => try mason.newPre("", .{}),

        // enums
        UnaryOp,
        BinaryOp,
        => try mason.newPre(@tagName(value), .{ .fg = theme.data }),

        // child nodes
        Node => try self.renderNode(mason, value),
        []const Node => nodes: {
            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            for (value) |node| {
                try divs.append(try self.renderNode(mason, node));
            }

            break :nodes try mason.newBox(divs.items, .{});
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("bad field data type: `{s}`", .{@typeName(T)});
            } else unreachable;
        },
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
        .unit => try mason.newBox(&.{
            tag,
            space,
            try mason.newPre("()", .{ .fg = theme.data }),
        }, .{ .direction = .right }),
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
        .call,
        .unary,
        .binary,
        .def,
        .program,
        => |data| div: {
            const Data = @TypeOf(data);
            const info = @typeInfo(Data);

            if (Data != Node and info == .Struct) {
                // render individual fields
                var fields = std.ArrayList(blox.Div).init(ally);
                defer fields.deinit();

                inline for (info.Struct.fields) |field| {
                    const field_name = try mason.newBox(&.{
                        try mason.newPre(field.name, .{ .fg = theme.field }),
                        try mason.newPre(":", .{}),
                    }, .{ .direction = .right });

                    const field_data = try self.renderFieldData(
                        mason,
                        field.type,
                        @field(data, field.name),
                    );

                    // fold name + data
                    const data_height = mason.getSize(field_data)[1];
                    const field_div = switch (data_height) {
                        0...1 => try mason.newBox(&.{
                            field_name,
                            space,
                            field_data,
                        }, .{ .direction = .right }),
                        else => try mason.newBox(&.{
                            field_name,
                            try mason.newBox(&.{
                                indent,
                                field_data,
                            }, .{ .direction = .right }),
                        }, .{}),
                    };

                    try fields.append(field_div);
                }

                // fold them together
                const data_div = try mason.newBox(fields.items, .{});

                break :div try mason.newBox(&.{
                    tag,
                    try mason.newBox(&.{
                        indent,
                        data_div,
                    }, .{ .direction = .right }),
                }, .{});
            } else {
                // directly render data for expr
                const data_div = try self.renderFieldData(mason, Data, data);
                const data_height = mason.getSize(data_div)[1];

                break :div switch (data_height) {
                    0...1 => try mason.newBox(&.{
                        tag,
                        space,
                        data_div,
                    }, .{ .direction = .right }),
                    else => try mason.newBox(&.{
                        tag,
                        try mason.newBox(&.{
                            indent,
                            data_div,
                        }, .{ .direction = .right }),
                    }, .{}),
                };
            }
        },
    };
}

pub fn render(self: *const Ast, mason: *blox.Mason) RenderError!blox.Div {
    if (self.root) |root| {
        return try self.renderNode(mason, root);
    }

    return try mason.newPre("<empty ast>", .{});
}
