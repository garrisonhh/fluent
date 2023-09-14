//! the set of all types

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Type = @import("type.zig").Type;

const TypeHashMapContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xBEEF_FA75;

    pub fn hash(_: @This(), t: *const Type) u64 {
        var hasher = Hasher.init(seed);
        std.hash.autoHash(&hasher, t.*);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: *const Type, b: *const Type) bool {
        return a.eql(b.*);
    }
};

const TypeHashMap = std.HashMapUnmanaged(
    *const Type,
    Type.Id,
    TypeHashMapContext,
    std.hash_map.default_max_load_percentage,
);

/// types which are cached on init
pub const PredefinedType = enum {
    const Self = @This();

    unit,
    bool,
    anyint,
    anyfloat,

    fn initType(self: Self) Type {
        return switch (self) {
            inline .unit,
            .bool,
            .anyint,
            .anyfloat,
            => |tag| @unionInit(Type, @tagName(tag), {}),
        };
    }
};

// interface ===================================================================

var map = com.RefMap(Type.Id, Type){};
var types = TypeHashMap{};
var predef_cache = std.enums.EnumMap(PredefinedType, Type.Id){};

pub fn init(ally: Allocator) Allocator.Error!void {
    for (std.enums.values(PredefinedType)) |p| {
        const id = try put(ally, p.initType());
        predef_cache.put(p, id);
    }
}

pub fn deinit(ally: Allocator) void {
    types.deinit(ally);

    var iter = map.iterator();
    while (iter.next()) |t| t.deinit(ally);
    map.deinit(ally);
}

/// retrieve a predefined type's id
pub fn predef(p: PredefinedType) Type.Id {
    return predef_cache.getAssertContains(p);
}

/// returns the unique id for this type, which may be newly generated or
/// retrieved from a previous put() call
pub fn put(ally: Allocator, init_type: Type) Allocator.Error!Type.Id {
    // find previous id
    const res = try types.getOrPut(ally, &init_type);
    if (res.found_existing) return res.value_ptr.*;

    // new id
    const id = try map.put(ally, init_type);
    res.key_ptr.* = map.get(id);
    res.value_ptr.* = id;

    return id;
}

pub fn get(id: Type.Id) *const Type {
    return map.get(id);
}
