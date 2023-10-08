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

    pub const Fn = struct {
        params: []const Type.Id,
        returns: Type.Id,
    };

    pub const Struct = struct {
        fields: []const Type.Id,
    };

    unit,
    type,
    ident,
    bool,
    int: Int,
    float: Float,
    @"fn": Fn,
    @"struct": Struct,

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .unit,
            .type,
            .ident,
            .bool,
            .int,
            .float,
            => {},

            .@"fn" => |f| {
                ally.free(f.params);
            },
            .@"struct" => |st| {
                ally.free(st.fields);
            },
        }
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        var owned = self;
        switch (owned) {
            .unit,
            .type,
            .ident,
            .bool,
            .int,
            .float,
            => {},

            .@"fn" => |*f| {
                f.params = try ally.dupe(Type.Id, f.params);
            },
            .@"struct" => |*st| {
                st.fields = try ally.dupe(Type.Id, st.fields);
            },
        }

        return owned;
    }
};
