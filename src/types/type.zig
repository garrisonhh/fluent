const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const blox = @import("blox");
const rendering = @import("rendering.zig");

pub const Type = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);
    pub const Id = com.Ref(.type, 32);

    /// needed for type system completeness
    unit,
    /// logic value
    bool,
    /// castable to any int
    anyint,
    /// castable to any float
    anyfloat,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = ally;
        switch (self) {
            .unit,
            .bool,
            .anyint,
            .anyfloat,
            => {},
        }
    }

    pub const RenderError = rendering.RenderError;

    pub const renderId = rendering.renderTypeId;

    pub fn render(self: Self, mason: *blox.Mason) RenderError!blox.Div {
        return rendering.renderType(mason, self);
    }

    pub fn eql(self: Self, other: Self) bool {
        return std.meta.eql(self, other);
    }
};
