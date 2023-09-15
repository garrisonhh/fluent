//! fluent source registry

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");

pub const Source = com.Ref(.source, 16);
const SourceMap = com.RefMap(Source, File);

pub const File = struct {
    name: []const u8,
    text: []const u8,
};

pub const Loc = packed struct(u64) {
    const Self = @This();

    source: Source,
    line_index: u32,
    char_index: u16,

    pub const RenderError = rendering.RenderError;
    pub const render = rendering.renderLoc;

    pub fn lineno(self: Self) usize {
        return self.line_index + 1;
    }

    pub fn charno(self: Self) usize {
        return self.char_index + 1;
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}:{d}:{d}", .{
            get(self.source).name,
            self.lineno(),
            self.charno(),
        });
    }
};

var map = SourceMap{};

pub fn deinit(ally: Allocator) void {
    var sources = map.iterator();
    while (sources.next()) |f| {
        ally.free(f.name);
        ally.free(f.text);
    }

    map.deinit(ally);

    // reset for tests
    if (builtin.is_test) {
        map = .{};
    }
}

pub fn add(
    ally: Allocator,
    name: []const u8,
    text: []const u8,
) Allocator.Error!Source {
    return try map.put(ally, .{
        .name = try ally.dupe(u8, name),
        .text = try ally.dupe(u8, text),
    });
}

pub fn get(src: Source) *const File {
    return map.get(src);
}

// rendering ===================================================================
