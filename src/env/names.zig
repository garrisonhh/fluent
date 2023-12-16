const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const builtin = @import("builtin");
const idents = @import("idents.zig");
const Ident = idents.Ident;

/// an interned series of idents corresponding to namespaces
pub const Name = com.Ref(.name, 64);

const HashMapContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xDEAD_BA75;

    pub fn hash(_: @This(), key: []const Ident) u64 {
        var hasher = Hasher.init(seed);
        // hasher.update(std.mem.sliceAsBytes(key));
        _ = key;
        return hasher.final();
    }

    pub fn eql(_: @This(), a: []const Ident, b: []const Ident) bool {
        if (a.len != b.len) {
            return false;
        }

        for (a, b) |elem_a, elem_b| {
            if (!elem_a.eql(elem_b)) {
                return false;
            }
        }

        return true;
    }
};

const HashMapUnmanaged = std.HashMapUnmanaged(
    []const Ident,
    Name,
    HashMapContext,
    std.hash_map.default_max_load_percentage,
);

var names = com.RefMap(Name, []const Ident){};
var set = HashMapUnmanaged{};

pub fn init() void {}

pub fn deinit(ally: Allocator) void {
    set.deinit(ally);

    var name_iter = names.iterator();
    while (name_iter.next()) |name_str| ally.free(name_str.*);
    names.deinit(ally);

    if (builtin.is_test) {
        names = .{};
        set = .{};
    }
}

/// get the slice for the name
pub fn slice(name: Name) []const Ident {
    return names.get(name).*;
}

/// returns a unique identifier for these idents
pub fn intern(ally: Allocator, buf: []const Ident) Allocator.Error!Name {
    const res = try set.getOrPut(ally, buf);
    if (!res.found_existing) {
        // recursively insert names down to root to ensure drop can't fail
        if (buf.len > 1) {
            _ = try intern(ally, buf[0 .. buf.len - 1]);
        }

        const owned = try ally.dupe(Ident, buf);
        const name = try names.put(ally, owned);

        res.key_ptr.* = owned;
        res.value_ptr.* = name;
    }

    return res.value_ptr.*;
}

/// drops an identifier from the name
pub fn drop(name: Name) ?Name {
    const buf = slice(name);
    if (buf.len == 0) return null;

    // guaranteed to by intern behavior
    return set.get(buf[0 .. buf.len - 1]).?;
}

/// adds an identifier to the name
pub fn push(ally: Allocator, name: Name, id: Ident) Allocator.Error!Name {
    const original = slice(name);
    const buf = try ally.alloc(Ident, original.len + 1);
    defer ally.free(buf);

    @memcpy(buf[0 .. buf.len - 1], original);
    buf[buf.len - 1] = id;

    return try intern(ally, buf);
}

// tests =======================================================================

test "name-interning" {
    const ally = std.testing.allocator;

    idents.init();
    defer idents.deinit(ally);

    init();
    defer deinit(ally);

    const hello = try idents.intern(ally, "hello");
    const world = try idents.intern(ally, "world");
    const god = try idents.intern(ally, "god");

    const hello_name = try intern(ally, &.{hello});
    const hello_world = try intern(ally, &.{ hello, world });
    const hello_god = try intern(ally, &.{ hello, god });
    const hello_world2 = try push(ally, hello_name, world);
    const hello_name2 = drop(hello_god) orelse {
        return error.TestFailure;
    };

    std.debug.assert(hello_world.eql(hello_world2));
    std.debug.assert(!hello_world.eql(hello_god));
    std.debug.assert(hello_name.eql(hello_name2));
}
