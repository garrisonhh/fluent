const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const blox = @import("blox");

pub const Type = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);
    pub const Id = com.Ref(.type, 32);

    pub const Int = struct {
        pub const Signedness = std.builtin.Signedness;

        pub const Bits = enum {
            @"8",
            @"16",
            @"32",
            @"64",
        };

        signedness: Signedness,
        bits: Bits,
    };

    pub const Float = struct {
        pub const Bits = enum {
            @"32",
            @"64",
        };

        bits: Bits,
    };

    unit,
    ident,
    bool,
    int: Int,
    float: Float,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = ally;
        switch (self) {
            .unit,
            .ident,
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
