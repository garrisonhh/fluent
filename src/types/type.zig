const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const blox = @import("blox");

pub const Type = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);
    pub const Id = com.Ref(.type, 32);

    any,
    unit,
    bool,
    // TODO bits
    int,
    // TODO bits
    float,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = ally;
        switch (self) {
            .any,
            .unit,
            .bool,
            .int,
            .float,
            => {},
        }
    }

    pub fn eql(self: Self, other: Self) bool {
        return std.meta.eql(self, other);
    }
};
