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

// TODO I'm using this pattern a lot, maybe I should impl a 2-way hashmap in
// common
const DefMap = std.AutoHashMapUnmanaged(Name, Value.Ref);
const ReverseMap = std.AutoHashMapUnmanaged(Value.Ref, Name);

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const ally = gpa.allocator();

var values: Value.RefList = .{};
/// pub for rendering only, don't touch this directly
pub var defs: DefMap = .{};
var reverse: ReverseMap = .{};

pub fn init() void {
    idents.init();
    names.init();
}

pub fn deinit() void {
    var val_iter = values.iterator();
    while (val_iter.next()) |v| v.deinit(ally);
    values.deinit(ally);

    defs.deinit(ally);
    reverse.deinit(ally);
    names.deinit(ally);
    idents.deinit(ally);

    _ = gpa.deinit();

    if (builtin.is_test) {
        gpa = .{};
        values = .{};
        defs = .{};
        reverse = .{};
    }
}

/// place a value in the env's memory context
///
/// *value ownership is moved to the env*
pub fn value(v: Value) Allocator.Error!Value.Ref {
    return try values.put(ally, v);
}

/// name a value
///
/// *value ownership is moved to the env*
pub fn add(n: Name, ref: Value.Ref) Allocator.Error!void {
    std.debug.assert(!defs.contains(n));

    try defs.put(ally, n, ref);
    try reverse.put(ally, ref, n);
}

/// create and add a value in one step
pub fn put(n: Name, v: Value) Allocator.Error!Value.Ref {
    const ref = try value(v);
    try add(n, ref);
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

/// intern an ident
pub fn ident(str: []const u8) Allocator.Error!Ident {
    return idents.intern(ally, str);
}

/// get the string backing an ident
pub const identStr = idents.str;

/// intern a name
pub fn name(buf: []const Ident) Allocator.Error!Name {
    return names.intern(ally, buf);
}

/// get the idents backing a name
pub const nameSlice = names.slice;

/// push an ident onto a name (make a child)
pub fn namePush(n: Name, id: Ident) Allocator.Error!Name {
    return names.push(ally, n, id);
}

/// drop an ident from a name (get the parent)
/// returns null if name is root
pub const nameDrop = names.drop;

pub fn nameIsRoot(n: Name) bool {
    return nameSlice(n).len == 0;
}

/// concatenate two names
pub fn nameCat(a: Name, b: Name) Allocator.Error!Name {
    const a_slice = nameSlice(a);
    if (a_slice.len == 0) return b;

    const b_slice = nameSlice(b);
    if (b_slice.len == 0) return a;

    var buf = try ally.alloc(Ident, a_slice.len + b_slice.len);
    defer ally.free(buf);

    @memcpy(buf[0..a_slice.len], a_slice);
    @memcpy(buf[a_slice.len..], b_slice);

    return try name(buf);
}

/// name from single string
pub fn nameFromStr(str: []const u8) Allocator.Error!Name {
    return try name(&.{
        try ident(str),
    });
}

// rendering ===================================================================

pub const renderIdent = rendering.renderIdent;
pub const renderName = rendering.renderName;
pub const renderValue = rendering.renderValueRef;
pub const render = rendering.renderEnv;

