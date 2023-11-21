const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const fluent = @import("../mod.zig");
const Ident = fluent.Ident;

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

            pub fn count(bits: Bits) u8 {
                return switch (bits) {
                    .@"8" => 8,
                    .@"16" => 16,
                    .@"32" => 32,
                    .@"64" => 64,
                };
            }
        };

        signedness: Signedness,
        bits: Bits,
    };

    pub const Float = struct {
        pub const Bits = enum {
            @"32",
            @"64",

            pub fn count(bits: Bits) u8 {
                return switch (bits) {
                    .@"32" => 32,
                    .@"64" => 64,
                };
            }
        };

        bits: Bits,
    };

    pub const Fn = struct {
        params: []const Type.Id,
        returns: Type.Id,
    };

    pub const Class = struct {
        pub const Member = struct {
            ident: Ident,
            type: Type.Id,
        };

        /// typeclasses define a number of declarations required in the type's
        /// namespace; e.g. consts + functions
        members: []const Member,
    };

    pub const Struct = struct {
        fields: []const Type.Id,
    };

    unit,
    type,
    name,
    bool,
    int: Int,
    float: Float,
    @"fn": Fn,
    /// typeclass
    class: Class,
    @"struct": Struct,

    pub fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .unit,
            .type,
            .name,
            .bool,
            .int,
            .float,
            => {},

            .@"fn" => |f| {
                ally.free(f.params);
            },
            .class => |cls| {
                ally.free(cls.members);
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
            .name,
            .bool,
            .int,
            .float,
            => {},

            .@"fn" => |*f| {
                f.params = try ally.dupe(Type.Id, f.params);
            },
            .class => |*cls| {
                cls.members = try ally.dupe(Class.Member, cls.members);
            },
            .@"struct" => |*st| {
                st.fields = try ally.dupe(Type.Id, st.fields);
            },
        }

        return owned;
    }
};
