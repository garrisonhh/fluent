const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");
const Loc = fluent.Loc;
const sources = fluent.sources;

pub const RenderError = blox.Error || std.fmt.AllocPrintError;

const theme = struct {
    const c = blox.Color.init;
    const meta = c(.normal, .cyan);
    const muted = c(.normal, .white);
};

const RenderLocContext = struct {
    const Line = struct {
        lineno: usize,
        text: []const u8,
    };

    filename: []const u8,
    lineno_render_width: usize,
    before: ?Line,
    this: Line,
    after: ?Line,
};

fn lineNoRenderWidth(lineno: usize) usize {
    var buf: [32]u8 = undefined;
    const mem = std.fmt.bufPrintIntToSlice(&buf, lineno, 10, .lower, .{});
    return mem.len;
}

/// gets all of the data needed to render a location with context
fn collectContext(loc: Loc) RenderLocContext {
    const Line = RenderLocContext.Line;
    const file = sources.get(loc.source);

    // collect lines
    var before: ?Line = null;
    var this: ?Line = null;
    var after: ?Line = null;

    var lines = std.mem.splitScalar(u8, file.text, '\n');
    var line_index: usize = 0;
    while (lines.next()) |line_text| : (line_index += 1) {
        const line = RenderLocContext.Line{
            .lineno = line_index + 1,
            .text = line_text,
        };

        if (line_index == loc.line_index) {
            this = line;
        } else if (line_index + 1 == loc.line_index) {
            before = line;
        } else if (line_index == loc.line_index + 1) {
            after = line;
        }
    }

    // collect metadata
    const lineno_render_width = 1 + std.mem.max(usize, &.{
        if (before) |line| lineNoRenderWidth(line.lineno) else 0,
        lineNoRenderWidth(this.?.lineno),
        if (after) |line| lineNoRenderWidth(line.lineno) else 0,
    });

    return RenderLocContext{
        .filename = file.name,
        .lineno_render_width = lineno_render_width,
        .before = before,
        .this = this.?,
        .after = after,
    };
}

fn renderAt(mason: *blox.Mason, loc: Loc) RenderError!blox.Div {
    const ally = mason.ally;

    const text = try std.fmt.allocPrint(ally, "{}", .{loc});
    defer ally.free(text);

    return try mason.newBox(&.{
        try mason.newPre("at ", .{ .fg = theme.muted }),
        try mason.newPre(text, .{ .fg = theme.muted }),
        try mason.newPre(":", .{ .fg = theme.muted }),
    }, .{ .direction = .right });
}

fn renderLine(
    mason: *blox.Mason,
    r: RenderLocContext,
    line: RenderLocContext.Line,
) RenderError!blox.Div {
    const ally = mason.ally;

    const lineno_text = try std.fmt.allocPrint(ally, "{d}", .{line.lineno});
    defer ally.free(lineno_text);

    const pad = r.lineno_render_width - lineno_text.len;
    const lineno_spacer = try mason.newSpacer(pad, 0, .{});

    return try mason.newBox(&.{
        lineno_spacer,
        try mason.newPre(lineno_text, .{ .fg = theme.meta }),
        try mason.newPre(" | ", .{ .fg = theme.meta }),
        try mason.newPre(line.text, .{}),
    }, .{ .direction = .right });
}

fn renderArrow(
    mason: *blox.Mason,
    r: RenderLocContext,
    char_index: usize,
) blox.Error!blox.Div {
    return try mason.newBox(&.{
        try mason.newSpacer(r.lineno_render_width, 0, .{}),
        try mason.newPre(" | ", .{ .fg = theme.meta }),
        try mason.newSpacer(char_index, 0, .{}),
        try mason.newPre("^", .{ .fg = theme.meta }),
    }, .{ .direction = .right });
}

/// renders a source location with context
pub fn renderLoc(loc: Loc, mason: *blox.Mason) RenderError!blox.Div {
    const ally = mason.ally;

    var divs = std.ArrayList(blox.Div).init(ally);
    defer divs.deinit();

    const r = collectContext(loc);

    try divs.append(try renderAt(mason, loc));

    if (r.before) |before| {
        try divs.append(try renderLine(mason, r, before));
    }

    try divs.append(try renderLine(mason, r, r.this));
    try divs.append(try renderArrow(mason, r, loc.char_index));

    if (r.after) |after| {
        try divs.append(try renderLine(mason, r, after));
    }

    return try mason.newBox(divs.items, .{});
}
