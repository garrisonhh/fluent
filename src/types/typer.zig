//! type system manager

// TODO make this managed, similar to env

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const Type = @import("type.zig").Type;
const TypeSet = @import("type_set.zig").TypeSet;
const prelude = @import("prelude.zig");
const fluent = @import("../mod.zig");

pub const PreludeType = prelude.PreludeType;
const SubclasserSet = std.AutoHashMapUnmanaged(Type.Id, void);

/// maps id -> type + metadata; owns everything
var map = com.RefMap(Type.Id, Type){};
/// set of type/id pairs
var types = TypeSet{};
/// maps type -> set of subclassers of the type
var graph = std.AutoHashMapUnmanaged(Type.Id, SubclasserSet){};

pub fn init(ally: Allocator) Allocator.Error!void {
    try prelude.init(ally);
}

pub fn deinit(ally: Allocator) void {
    types.deinit(ally);

    var iter = map.iterator();
    while (iter.next()) |t| t.deinit(ally);
    map.deinit(ally);

    var graph_sets = graph.valueIterator();
    while (graph_sets.next()) |set| set.deinit(ally);
    graph.deinit(ally);

    prelude.deinit(ally);

    // reset for tests
    if (builtin.is_test) {
        map = .{};
        types = .{};
        graph = .{};
    }
}

pub const RenderError = rendering.RenderError;
pub const render = rendering.renderTypeId;

/// retrieve a prelude type
pub fn pre(pt: PreludeType) Type.Id {
    return prelude.get(pt);
}

/// mark a type as a subclass of another type
pub fn addClass(
    ally: Allocator,
    t: Type.Id,
    super: Type.Id,
) Allocator.Error!void {
    // get the type set for super, or create it if it doesn't exist
    const res = try graph.getOrPut(ally, super);
    if (!res.found_existing) {
        res.value_ptr.* = .{};
    }
    const set = res.value_ptr;

    // add subclasser and subclasser's subclassers
    // the class lists should always be flat, so I don't have to do this
    // recursively
    try set.put(ally, t, {});

    if (graph.getPtr(t)) |subset| {
        var subs = subset.keyIterator();
        while (subs.next()) |sub| {
            try set.put(ally, sub.*, {});
        }
    }
}

pub fn isSubclass(t: Type.Id, super: Type.Id) bool {
    const set = graph.getPtr(super) orelse {
        std.debug.assert(super.eql(pre(.never)));
        return false;
    };

    // class lists are always flat, so this doesn't have to be recursive
    return set.contains(t);
}

pub const AdvancedTypeOptions = struct {
    subclasses_any: bool = true,
    never_subclasses: bool = true,
};

/// creates an invalid type id for potentially self-referential types
///
/// *you must call `setSelfRef` on the id before it is accessed*
pub fn newType(ally: Allocator) Allocator.Error!Type.Id {
    return try map.new(ally);
}

/// store a possibly self-referential type with a `this` id created using
/// `newSelfRef`, returning the intern'd type id
///
/// *type ownership is moved on calling this*
pub fn setType(
    ally: Allocator,
    this: Type.Id,
    init_type: Type,
    options: AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    // create type
    const res = try types.getOrPut(ally, .{
        .id = this,
        // remember this is a dangling pointer
        .type = &init_type,
    });

    if (res.found_existing) {
        // this type already exists
        init_type.deinit(ally);
    } else {
        try map.set(ally, this, init_type);
        res.key_ptr.* = .{
            .id = this,
            .type = map.get(this),
        };
    }

    const t = res.key_ptr.id;

    // apply top and bottom types
    if (options.subclasses_any) try addClass(ally, t, pre(.any));
    if (options.never_subclasses) try addClass(ally, pre(.never), t);

    return t;
}

/// create a type (convenient for types that can't be self referential)
///
/// *type ownership is moved on calling this*
pub fn put(ally: Allocator, t: Type) Allocator.Error!Type.Id {
    return try putAdvanced(ally, t, .{});
}

/// create a type (convenient for types that can't be self referential)
///
/// *type ownership is moved on calling this*
pub fn putAdvanced(
    ally: Allocator,
    t: Type,
    options: AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    errdefer t.deinit(ally);
    return try setType(ally, try newType(ally), t, options);
}

pub fn get(id: Type.Id) *const Type {
    return map.get(id);
}

// debugging ===================================================================

fn dumpTypeImpl(t: Type, this: Type.Id) !void {
    if (builtin.mode != .Debug) {
        @compileError("don't leave this in release code");
    }

    const blox = @import("blox");
    const ally = std.heap.page_allocator;
    const stderr = std.io.getStdErr().writer();

    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    const div = try mason.newBox(&.{
        try mason.newPre("[dump]", .{}),
        try rendering.renderType(&mason, t, this),
        try mason.newSpacer(0, 1, .{}),
    }, .{});

    try mason.write(div, stderr, .{});
}

/// dump a type to stderr in debug mode
pub fn dumpType(t: Type, this: Type.Id) void {
    dumpTypeImpl(t, this) catch |e| {
        std.debug.panic("error in dumpType: {s}", .{@errorName(e)});
    };
}