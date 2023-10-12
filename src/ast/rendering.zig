const std = @import("std");
const builtin = @import("builtin");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Type = fluent.Type;
const typer = fluent.typer;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const span = blox.BoxOptions{ .direction = .right };

const theme = struct {
    const c = blox.Color.init;

    const tag = c(.bright, .cyan);
    const data = c(.bright, .magenta);
    const field = c(.normal, .yellow);
    const err = c(.bright, .red);
};

// errors ======================================================================

const ErrorScope = enum {
    syntax,
    type,
};

/// adds tag to error description
fn errorDesc(
    mason: *blox.Mason,
    scope: ErrorScope,
    desc: blox.Div,
) RenderError!blox.Div {
    const ally = mason.ally;

    const err_name = try std.fmt.allocPrint(ally, "{s} error", .{
        @tagName(scope),
    });
    defer ally.free(err_name);

    return try mason.newBox(&.{
        try mason.newPre("[", .{}),
        try mason.newPre(err_name, .{ .fg = theme.err }),
        try mason.newPre("] ", .{}),
        desc,
    }, span);
}

/// an error with a description and a location
fn simpleError(
    mason: *blox.Mason,
    scope: ErrorScope,
    loc: Loc,
    comptime fmt: []const u8,
    args: anytype,
) RenderError!blox.Div {
    const ally = mason.ally;
    const text = try std.fmt.allocPrint(ally, fmt, args);
    defer ally.free(text);

    return try mason.newBox(&.{
        try errorDesc(mason, scope, try mason.newPre(text, .{})),
        try loc.render(mason),
    }, .{});
}

fn renderSyntaxError(
    mason: *blox.Mason,
    meta: fluent.SyntaxErrorMeta,
) RenderError!blox.Div {
    return switch (meta) {
        .unexpected_eof => |loc| try simpleError(
            mason,
            .syntax,
            loc,
            "unexpected EOF",
            .{},
        ),

        .invalid_literal => |il| try simpleError(
            mason,
            .syntax,
            il.loc,
            "invalid {s} literal",
            .{@tagName(il.tag)},
        ),

        .expected_op_expr => |exp| try simpleError(
            mason,
            .syntax,
            exp.loc,
            "expected expression for operator `{s}`",
            .{exp.operator},
        ),

        .expected_token => |exp| try simpleError(
            mason,
            .syntax,
            exp.loc,
            "expected {s}",
            .{@tagName(exp.tag)},
        ),

        .expected_one_of => |exp| e: {
            var list = std.ArrayList(u8).init(mason.ally);
            defer list.deinit();
            const writer = list.writer();

            std.debug.assert(exp.tags.len >= 2);

            const tags = exp.tags;

            for (tags[0 .. tags.len - 2]) |tag| {
                try writer.print("{s}, ", .{@tagName(tag)});
            }

            try writer.print("{s} or {s}", .{
                @tagName(tags[tags.len - 2]),
                @tagName(tags[tags.len - 1]),
            });

            break :e try simpleError(
                mason,
                .syntax,
                exp.loc,
                "expected one of {s}",
                .{list.items},
            );
        },

        .expected_desc => |exp| try simpleError(
            mason,
            .syntax,
            exp.loc,
            "expected {s}",
            .{exp.desc},
        ),
    };
}

fn renderSemaError(
    mason: *blox.Mason,
    meta: fluent.SemaErrorMeta,
) RenderError!blox.Div {
    return switch (meta) {
        .expected => |exp| try mason.newBox(&.{
            try errorDesc(mason, .type, try mason.newBox(&.{
                try mason.newPre("expected ", .{}),
                try typer.render(mason, exp.expected),
                try mason.newPre(", found ", .{}),
                try typer.render(mason, exp.found),
            }, span)),
            try exp.loc.render(mason),
        }, .{}),

        .expected_one_of => |exp| exp: {
            const comma = try mason.newPre(", ", .{});

            var desc_divs = std.ArrayList(blox.Div).init(mason.ally);
            defer desc_divs.deinit();

            try desc_divs.append(try mason.newPre("expected one of: ", .{}));

            for (exp.expected, 0..) |t, i| {
                if (i > 0) try desc_divs.append(comma);
                try desc_divs.append(try typer.render(mason, t));
            }

            const desc = try mason.newBox(&.{
                try mason.newBox(desc_divs.items, span),
                try mason.newBox(&.{
                    try mason.newPre("found: ", .{}),
                    try typer.render(mason, exp.found),
                }, span),
            }, .{});

            break :exp try mason.newBox(&.{
                try errorDesc(mason, .type, desc),
                try exp.loc.render(mason),
            }, .{});
        },

        .expected_matching => |exp| try mason.newBox(&.{
            try errorDesc(mason, .type, try mason.newBox(&.{
                try mason.newPre("expected ", .{}),
                try typer.render(mason, exp.expected),
                try mason.newPre(", found ", .{}),
                try typer.render(mason, exp.found),
            }, span)),
            try exp.found_loc.render(mason),
            try mason.newPre("this would match the expression here", .{}),
            try exp.expected_loc.render(mason),
        }, .{}),
    };
}

pub fn renderAstError(
    self: Ast.Error,
    mason: *blox.Mason,
) RenderError!blox.Div {
    return switch (self) {
        .syntax => |meta| try renderSyntaxError(mason, meta),
        .semantic => |meta| try renderSemaError(mason, meta),
    };
}

// ast =========================================================================

fn renderFieldData(
    self: *const Ast,
    mason: *blox.Mason,
    comptime T: type,
    value: T,
) RenderError!blox.Div {
    const ally = mason.ally;
    return switch (T) {
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
        []const Ast.Expr.RecordEntry => kvs: {
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
    }, span);

    return switch (expr) {
        .value => |v| try fluent.env.renderValue(mason, fluent.env.get(v).*),

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
