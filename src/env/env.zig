const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const idents = @import("idents.zig");
const names = @import("names.zig");
const rendering = @import("rendering.zig");

pub const Value = @import("value.zig").Value;
pub const Ident = idents.Ident;
pub const Name = names.Name;

const DefMap = com.RefMap(Name, Value);

var defs: DefMap = .{};

pub fn init() void {
    idents.init();
    names.init();
}

pub fn deinit(ally: Allocator) void {
    var def_iter = defs.iterator();
    while (def_iter.next()) |value| value.deinit(ally);
    defs.deinit(ally);

    names.deinit(ally);
    idents.deinit(ally);

    if (builtin.is_test) {
        defs = .{};
    }
}

/// add an entry to the env
///
/// *value ownership is moved to the env*
pub fn add(ally: Allocator, n: Name, value: Value) Allocator.Error!void {
    try defs.put(ally, n, value);
}

pub fn get(n: Name) ?*const Value {
    return getMut(n);
}

pub fn getMut(n: Name) ?*Value {
    return defs.getOpt(n);
}

// ident/name behavior =========================================================

/// intern an ident
pub const ident = idents.intern;
/// get the string backing an ident
pub const identStr = idents.str;

/// intern a name
pub const name = names.intern;
/// get the idents backing a name
pub const nameSlice = names.slice;
/// push an ident onto a name (make a child)
pub const namePush = names.push;
/// drop an ident from a name (get the parent)
pub const nameDrop = names.drop;

pub const renderIdent = rendering.renderIdent;
pub const renderName = rendering.renderName;