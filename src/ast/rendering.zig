const std = @import("std");
const builtin = @import("builtin");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Type = fluent.Type;
const typer = fluent.typer;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const theme = struct {
    const c = blox.Color.init;

    const tag = c(.bright, .cyan);
    const data = c(.bright, .magenta);
    const field = c(.normal, .yellow);
    const err = c(.bright, .red);
};

pub fn renderAstError(
    self: Ast.Error,
    mason: *blox.Mason,
) RenderError!blox.Div {
    var divs = std.BoundedArray(blox.Div, 2){};

    const desc = try mason.newBox(&.{
        try mason.newPre("[", .{}),
        try mason.newPre("error", .{ .fg = theme.err }),
        try mason.newPre("] ", .{}),
        self.desc,
    }, .{ .direction = .right });

    divs.appendAssumeCapacity(desc);

    if (self.loc) |loc| {
        divs.appendAssumeCapacity(try fluent.sources.render(mason, loc));
    }

    return try mason.newBox(divs.slice(), .{});
}

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
) RenderError!blox.Div {
    const indent = try mason.newSpacer(2, 0, .{});
    const space = try mason.newSpacer(1, 0, .{});

    // rendering
    const ally = mason.ally;
    const expr = self.get(node).*;

    const typing = if (self.getType(node)) |t|
        try typer.render(mason, t)
    else
        try mason.newPre("untyped", .{ .fg = theme.err });

    const tag = try mason.newPre(@tagName(expr), .{ .fg = theme.tag });

    const label = try mason.newBox(&.{
        tag,
        try mason.newPre(" <", .{}),
        typing,
        try mason.newPre(">", .{}),
    }, .{ .direction = .right });

    return switch (expr) {
        // literals
        .unit => try mason.newBox(&.{
            label,
            space,
            try mason.newPre("()", .{ .fg = theme.data }),
        }, .{ .direction = .right }),
        .ident => |ident| try mason.newBox(&.{
            label,
            space,
            try mason.newPre(ident, .{}),
        }, .{ .direction = .right }),

        inline .int, .real => |n| n: {
            const text = try std.fmt.allocPrint(ally, "{d}", .{n});
            defer ally.free(text);

            break :n try mason.newBox(&.{
                label,
                space,
                try mason.newPre(text, .{ .fg = theme.data }),
            }, .{ .direction = .right });
        },

        // containers
        inline .parens,
        .call,
        .unary,
        .binary,
        .program,
        .let,
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
                    }, .{ .direction = .right });

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
                    label,
                    try mason.newBox(&.{
                        indent,
                        data_div,
                    }, .{ .direction = .right }),
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
                    }, .{ .direction = .right }),
                    else => try mason.newBox(&.{
                        label,
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
