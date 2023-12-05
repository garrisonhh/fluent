//! fluent source registry

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const fluent = @import("mod.zig");
const env = fluent.env;
const Name = fluent.Name;
const rendering = @import("rendering.zig");

pub const Source = com.Ref(.source, 16);
const SourceMap = com.RefMap(Source, File);

pub const File = struct {
    filename: []const u8,
    name: Name,
    text: []const u8,
};

pub const Loc = packed struct(u64) {
    const Self = @This();

    source: Source,
    line_index: u32,
    char_index: u16,

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
            get(self.source).filename,
            self.lineno(),
            self.charno(),
        });
    }
};

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const ally = gpa.allocator();

var map = SourceMap{};

pub fn deinit() void {
    var sources = map.iterator();
    while (sources.next()) |f| {
        ally.free(f.filename);
        ally.free(f.text);
    }

    map.deinit(ally);

    // reset for tests
    if (builtin.is_test) {
        map = .{};
        gpa = .{};
    }
}

/// create a canonical namespace name from the filename
fn canonicalName(filename: []const u8) Allocator.Error!Name {
    const dot = std.mem.indexOf(u8, filename, ".") orelse filename.len;
    return try env.nameFromStr(filename[0..dot]);
}

pub fn add(filename: []const u8, text: []const u8) Allocator.Error!Source {
    return try map.put(ally, .{
        .filename = try ally.dupe(u8, filename),
        .name = try canonicalName(filename),
        .text = try ally.dupe(u8, text),
    });
}

pub fn get(src: Source) *const File {
    return map.get(src);
}

/// sometimes in tests you need to create an ast node but you don't have a
/// source
pub fn testLoc() Allocator.Error!Loc {
    const Ns = struct {
        var test_source: ?Source = null;
    };

    if (Ns.test_source == null) {
        Ns.test_source = try add(
            "virtual-test-source.fl",
            "this is a virtual test source file",
        );
    }

    return Loc{
        .source = Ns.test_source.?,
        .line_index = 0,
        .char_index = 0,
    };
}
