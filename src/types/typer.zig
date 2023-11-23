//! type system manager

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const Type = @import("type.zig").Type;
const TypeSet = @import("type_set.zig").TypeSet;
const TypeGraph = @import("TypeGraph.zig");
const prelude = @import("prelude.zig");
const fluent = @import("../mod.zig");

pub const PreludeType = prelude.PreludeType;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const ally = gpa.allocator();

/// maps id -> type + metadata; owns everything
var map = com.RefMap(Type.Id, Type){};
/// set of type/id pairs
var types = TypeSet{};
/// maps types to their super and subtypes
var graph: TypeGraph = undefined;

pub fn init() Allocator.Error!void {
    graph = try TypeGraph.init(ally);
    try prelude.init();
}

pub fn deinit() void {
    types.deinit(ally);

    var iter = map.iterator();
    while (iter.next()) |t| t.deinit(ally);
    map.deinit(ally);

    prelude.deinit(ally);
    graph.deinit(ally);

    _ = gpa.deinit();

    // reset for tests
    if (builtin.is_test) {
        gpa = .{};
        map = .{};
        types = .{};
    }
}

pub const RenderError = rendering.RenderError;
pub const render = rendering.renderTypeId;
pub const renderAnonymous = rendering.renderAnonymousTypeId;

/// retrieve a prelude type
pub fn pre(pt: PreludeType) Type.Id {
    return prelude.get(pt);
}

/// mark a type as a subclass of another type
pub fn addClass(t: Type.Id, super: Type.Id) Allocator.Error!void {
    try graph.addSupertype(ally, t, super);
}

pub fn isSubtype(t: Type.Id, super: Type.Id) bool {
    return graph.isSubtype(t, super);
}

pub fn isSupertype(t: Type.Id, sub: Type.Id) bool {
    return graph.isSupertype(t, sub);
}

/// if inner is narrower or equal to outer
pub fn isCompatible(inner: Type.Id, outer: Type.Id) bool {
    return inner.eql(outer) or isSubtype(inner, outer);
}

/// create a typeclass which is the union of two types, either or both of which
/// may also be typeclasses
pub fn mergePair(a: Type.Id, b: Type.Id) Allocator.Error!Type.Id {
    // directional compatibility
    if (isCompatible(a, b)) {
        return a;
    } else if (isSubtype(b, a)) {
        return b;
    }

    // must merge
    const merged = try put(.{ .class = .{ .members = &.{} } });

    try addClass(a, merged);
    try addClass(b, merged);

    return merged;
}

/// merges the types of some number of nodes
pub fn merge(ts: []const Type.Id) Allocator.Error!Type.Id {
    if (ts.len == 0) {
        return pre(.never);
    }

    var merged = ts[0];
    for (ts[1..]) |t| {
        merged = try mergePair(merged, t);
    }

    return merged;
}

pub const AdvancedTypeOptions = struct {
    distinct: bool = true,
    subclasses_any: bool = true,
    never_subclasses: bool = true,
};

/// creates an invalid type id for potentially self-referential types
///
/// *you must call `setSelfRef` on the id before it is accessed*
pub fn newType() Allocator.Error!Type.Id {
    return try map.new(ally);
}

/// store a possibly self-referential type with a `this` id created using
/// `newSelfRef`, returning the intern'd type id
/// TODO automagically correct the type graph
pub fn setType(
    this: Type.Id,
    init_type: Type,
    options: AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    if (options.distinct) {
        // create distinct type
        const t = try init_type.clone(ally);

        try map.set(ally, this, t);
        try types.put(ally, .{
            .id = this,
            .type = map.get(this),
        }, {});
    } else {
        const res = try types.getOrPut(ally, .{
            .id = this,
            // remember this is a dangling pointer
            .type = &init_type,
        });

        if (res.found_existing) {
            // this type already exists
            map.delUnused(this);

            return res.key_ptr.id;
        } else {
            const t = try init_type.clone(ally);

            try map.set(ally, this, t);
            res.key_ptr.* = .{
                .id = this,
                .type = map.get(this),
            };
        }
    }

    // apply top and bottom types
    if (options.subclasses_any) try addClass(this, pre(.any));
    if (options.never_subclasses) try addClass(pre(.never), this);

    return this;
}

/// intern a type (convenient for types that can't be self referential)
/// defaults to indistinct types
pub fn put(t: Type) Allocator.Error!Type.Id {
    return try putAdvanced(t, .{
        .distinct = false,
    });
}

/// create a type (convenient for types that can't be self referential)
pub fn putAdvanced(
    t: Type,
    options: AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    return try setType(try newType(), t, options);
}

pub fn get(id: Type.Id) *const Type {
    return map.get(id);
}

// debugging ===================================================================

const blox = @import("blox");
const stderr = std.io.getStdErr().writer();
const span = blox.BoxOptions{ .direction = .right };

fn dumpImpl() !void {
    if (builtin.mode != .Debug) {
        @compileError("don't leave this in release code");
    }

    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    var rows = std.ArrayList(blox.Div).init(ally);
    defer rows.deinit();

    try rows.append(try mason.newPre("[type dump]", .{}));

    var iter = map.iterator();
    while (iter.nextEntry()) |entry| {
        const t = entry.ref;
        var subtypes = std.ArrayList(blox.Div).init(mason.ally);
        defer subtypes.deinit();
        var supertypes = std.ArrayList(blox.Div).init(mason.ally);
        defer supertypes.deinit();

        try subtypes.append(try mason.newPre("  :>", .{}));
        try supertypes.append(try mason.newPre("  <:", .{}));

        var sub_iter = graph.subtypes(t);
        while (sub_iter.next()) |sub| {
            try subtypes.appendSlice(&.{
                try mason.newSpacer(1, 1, .{}),
                try render(&mason, sub),
            });
        }

        var super_iter = graph.supertypes(t);
        while (super_iter.next()) |super| {
            try supertypes.appendSlice(&.{
                try mason.newSpacer(1, 1, .{}),
                try render(&mason, super),
            });
        }

        const label = try mason.newBox(&.{
            try render(&mason, t),
            try mason.newPre(" = ", .{}),
            try renderAnonymous(&mason, t),
        }, span);

        try rows.append(try mason.newBox(&.{
            label,
            try mason.newBox(subtypes.items, span),
            try mason.newBox(supertypes.items, span),
        }, .{}));
    }

    const div = try mason.newBox(rows.items, .{});

    try mason.write(div, stderr, .{});
}

/// dump typer
pub fn dump() void {
    dumpImpl() catch {};
}
