const std = @import("std");
const builtin = @import("builtin");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const typer = fluent.typer;

const span = blox.BoxOptions{ .direction = .right };

const theme = struct {
    const c = blox.Color.init;

    const tag = c(.bright, .cyan);
    const data = c(.bright, .magenta);
    const field = c(.normal, .yellow);
    const err = c(.bright, .black);
};

fn renderFieldData(
    self: *const Ast,
    mason: *blox.Mason,
    comptime T: type,
    value: T,
) blox.Error!blox.Div {
    const ally = mason.ally;
    return switch (T) {
        // literals
        void => try mason.newPre("()", .{ .fg = theme.data }),
        bool => try mason.newPre(
            if (value) "true" else "false",
            .{ .fg = theme.data },
        ),
        Ast.Expr.Number => try mason.newPre(value.str, .{ .fg = theme.data }),
        fluent.Ident => fluent.env.renderIdent(mason, value),

        // enums
        Ast.UnaryOp,
        Ast.BinaryOp,
        => try mason.newPre(@tagName(value), .{ .fg = theme.data }),

        // child nodes
        Ast.Node => try self.render(mason, value),
        []const Ast.Node => nodes: {
            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            for (value) |node| {
                try divs.append(try self.render(mason, node));
            }

            break :nodes try mason.newBox(divs.items, .{});
        },
        []const Ast.Expr.KV => kvs: {
            const entry_tag = try mason.newPre("entry", .{ .fg = theme.tag });
            const indent = try mason.newSpacer(2, 1, .{});

            var divs = std.ArrayList(blox.Div).init(ally);
            defer divs.deinit();

            for (value) |entry| {
                try divs.append(try mason.newBox(&.{
                    entry_tag,
                    try mason.newBox(&.{
                        indent,
                        try mason.newBox(&.{
                            try self.render(mason, entry.key),
                            try self.render(mason, entry.value),
                        }, .{}),
                    }, span),
                }, .{}));
            }

            break :kvs try mason.newBox(divs.items, .{});
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("bad field data type: `{s}`", .{@typeName(T)});
            } else unreachable;
        },
    };
}

pub fn render(
    self: *const Ast,
    mason: *blox.Mason,
    node: Ast.Node,
) blox.Error!blox.Div {
    const indent = try mason.newSpacer(2, 0, .{});
    const space = try mason.newSpacer(1, 0, .{});

    // rendering
    const ally = mason.ally;
    const expr = self.get(node).*;

    const typing = if (self.getTypeOpt(node)) |t|
        try typer.render(mason, t)
    else
        try mason.newPre("untyped", .{ .fg = theme.err });

    const tag = try mason.newPre(@tagName(expr), .{ .fg = theme.tag });

    const label = try mason.newBox(&.{
        tag,
        try mason.newPre(" <", .{}),
        typing,
        try mason.newPre("> ", .{}),
    }, span);

    return switch (expr) {
        inline .unit,
        .bool,
        .number,
        .ident,
        => |data| try mason.newBox(&.{
            label,
            try renderFieldData(self, mason, @TypeOf(data), data),
        }, span),

        inline .parens,
        .record,
        .call,
        .unary,
        .binary,
        .program,
        .@"fn",
        .@"if",
        => |data| div: {
            const Data = @TypeOf(data);
            const info = @typeInfo(Data);

            if (Data != Ast.Node and info == .Struct) {
                // render individual fields
                var fields = std.ArrayList(blox.Div).init(ally);
                defer fields.deinit();

                inline for (info.Struct.fields) |field| {
                    const field_name = try mason.newBox(&.{
                        try mason.newPre(field.name, .{ .fg = theme.field }),
                        try mason.newPre(":", .{}),
                    }, span);

                    const field_data = try renderFieldData(
                        self,
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
                        }, span),
                        else => try mason.newBox(&.{
                            field_name,
                            try mason.newBox(&.{ indent, field_data }, span),
                        }, .{}),
                    };

                    try fields.append(field_div);
                }

                // fold them together
                const data_div = try mason.newBox(fields.items, .{});

                break :div try mason.newBox(&.{
                    label,
                    try mason.newBox(&.{ indent, data_div }, span),
                }, .{});
            } else {
                // directly render data for expr
                const data_div = try renderFieldData(self, mason, Data, data);
                const data_height = mason.getSize(data_div)[1];

                break :div switch (data_height) {
                    0...1 => try mason.newBox(&.{
                        label,
                        space,
                        data_div,
                    }, span),
                    else => try mason.newBox(&.{
                        label,
                        try mason.newBox(&.{ indent, data_div }, span),
                    }, .{}),
                };
            }
        },
    };
}
