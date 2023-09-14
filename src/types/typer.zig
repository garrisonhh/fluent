//! the set of all types

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const Type = @import("type.zig").Type;

const TypeHashMapContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xBEEF_FA75;

    fn eql(_: @This(), a: *const Type, b: *const Type) bool {
        return a.eql(b.*);
    }

    fn hash(_: @This(), t: *const Type) u64 {
        var hasher = Hasher.init(seed);
        std.hash.autoHash(&hasher, t.*);
        return hasher.final();
    }
};

const TypeHashMap = std.HashMapUnmanaged(
    *const Type,
    Type.Id,
    TypeHashMapContext,
    std.hash_map.default_max_load_percentage,
);

var map = com.RefMap(Type.Id, Type){};
var types = TypeHashMap{};

pub fn deinit(ally: Allocator) void {
    map.deinit(ally);
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
