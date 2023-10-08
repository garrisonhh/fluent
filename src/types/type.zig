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

    pub const Named = struct {
        name: []const u8,
        type: Id,
    };

    pub const Fn = struct {
        // TODO these shouldn't be named in the type, that's a scope thing
        params: []const Named,
        returns: Type.Id,
    };

    pub const Struct = struct {
        // TODO should these be named in the type? maybe would be better to
        // make that a scoping responsibility
        fields: []const Named,
    };

    unit,
    type,
    ident,
    bool,
    int: Int,
    float: Float,
    @"fn": Fn,
    @"struct": Struct,

    fn freeNameds(ally: Allocator, nameds: []const Named) void {
        for (nameds) |named| ally.free(named.name);
        ally.free(nameds);
    }

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
                freeNameds(ally, f.params);
            },
            .@"struct" => |st| {
                freeNameds(ally, st.fields);
            },
        }
    }

    pub fn cloneNameds(
        ally: Allocator,
        nameds: []const Named,
    ) Allocator.Error![]const Named {
        const owned = try ally.alloc(Named, nameds.len);
        for (owned, nameds) |*slot, named| {
            slot.* = .{
                .name = try ally.dupe(u8, named.name),
                .type = named.type,
            };
        }

        return owned;
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
                f.params = try cloneNameds(ally, f.params);
            },
            .@"struct" => |*st| {
                st.fields = try cloneNameds(ally, st.fields);
            },
        }

        return owned;
    }
};
