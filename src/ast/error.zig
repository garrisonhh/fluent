//! ast error implementation

const std = @import("std");
const Allocator = std.mem.Allocator;
const rendering = @import("rendering.zig");
const fluent = @import("../mod.zig");
const Loc = fluent.Loc;

/// error metadata for rendering
pub const Error = union(enum) {
    const Self = @This();

    syntax: fluent.SyntaxErrorMeta,
    semantic: fluent.SemaErrorMeta,

    pub fn deinit(self: Error, ally: Allocator) void {
        _ = self;
        _ = ally;
    }

    pub const render = rendering.renderAstError;
};

