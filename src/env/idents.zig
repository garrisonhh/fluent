const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const builtin = @import("builtin");

/// an interned string type
pub const Ident = com.Ref(.identifier, 64);

var idents = com.RefMap(Ident, []const u8){};
var set = std.StringHashMapUnmanaged(Ident){};

pub fn init() void {}

pub fn deinit(ally: Allocator) void {
    set.deinit(ally);

    var ident_iter = idents.iterator();
    while (ident_iter.next()) |ident_str| ally.free(ident_str.*);
    idents.deinit(ally);

    if (builtin.is_test) {
        idents = .{};
        set = .{};
    }
}

/// get the string for the ident
pub fn str(ident: Ident) []const u8 {
    return idents.get(ident).*;
}

/// returns a unique identifier for this series of bytes
pub fn intern(ally: Allocator, ident_str: []const u8) Allocator.Error!Ident {
    const res = try set.getOrPut(ally, ident_str);
    if (!res.found_existing) {
        const owned = try ally.dupe(u8, ident_str);
        const ident = try idents.put(ally, owned);

        res.key_ptr.* = owned;
        res.value_ptr.* = ident;
    }

    return res.value_ptr.*;
}

// tests =======================================================================

test "ident-interning" {
    const ally = std.testing.allocator;

    init();
    defer deinit(ally);

    const a = try intern(ally, "hello");
    const b = try intern(ally, "world");
    const c = try intern(ally, "hello");

    try std.testing.expect(a.eql(c));
    try std.testing.expect(!a.eql(b));
    try std.testing.expect(!b.eql(c));
    try std.testing.expectEqualSlices(u8, str(a), str(c));
}
