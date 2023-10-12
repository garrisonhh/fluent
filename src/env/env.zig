const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const idents = @import("idents.zig");
const Ident = idents.Ident;
const names = @import("names.zig");
const Name = names.Name;
const Value = @import("value.zig").Value;
const rendering = @import("rendering.zig");

// TODO env should probably just be managed

// TODO I'm using this pattern a lot, maybe I should impl a 2-way hashmap in
// common
const DefMap = std.AutoHashMapUnmanaged(Name, Value.Ref);
const ReverseMap = std.AutoHashMapUnmanaged(Value.Ref, Name);

var values: Value.RefList = .{};
/// pub for rendering only, don't touch this directly
pub var defs: DefMap = .{};
var reverse: ReverseMap = .{};

pub fn init() void {
    idents.init();
    names.init();
}

pub fn deinit(ally: Allocator) void {
    var val_iter = values.iterator();
    while (val_iter.next()) |v| v.deinit(ally);
    values.deinit(ally);

    defs.deinit(ally);
    reverse.deinit(ally);
    names.deinit(ally);
    idents.deinit(ally);

    if (builtin.is_test) {
        values = .{};
        defs = .{};
        reverse = .{};
    }
}

/// place a value in the env's memory context
///
/// *value ownership is moved to the env*
pub fn value(ally: Allocator, v: Value) Allocator.Error!Value.Ref {
    return try values.put(ally, v);
}

/// name a value
///
/// *value ownership is moved to the env*
pub fn add(ally: Allocator, n: Name, ref: Value.Ref) Allocator.Error!void {
    std.debug.assert(!defs.contains(n));

    try defs.put(ally, n, ref);
    try reverse.put(ally, ref, n);
}

/// create and add a value in one step
pub fn put(ally: Allocator, n: Name, v: Value) Allocator.Error!Value.Ref {
    const ref = try value(ally, v);
    try add(ally, n, ref);
    return ref;
}

pub fn get(ref: Value.Ref) *const Value {
    return values.get(ref);
}

pub fn getMut(ref: Value.Ref) *Value {
    return values.get(ref);
}

/// if a name is defined, get its value
pub fn lookup(n: Name) ?Value.Ref {
    return defs.get(n);
}

/// if a name is defined, get its type
pub fn lookupType(n: Name) ?Type.Id {
    if (lookup(n)) |ref| {
        return get(ref).findType();
    }

    return null;
}

/// look up a value's name, if it has one
pub fn reverseLookup(ref: Value.Ref) ?Name {
    return reverse.get(ref);
}

// ident/name behavior =========================================================

// TODO this interface is meh, I can make it more usable when I make env
// memory managed

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

// rendering ===================================================================

pub const renderIdent = rendering.renderIdent;
pub const renderName = rendering.renderName;
pub const renderValue = rendering.renderValueRef;
pub const render = rendering.renderEnv;

