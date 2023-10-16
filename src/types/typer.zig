//! type system manager

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

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const ally = gpa.allocator();

/// maps id -> type + metadata; owns everything
var map = com.RefMap(Type.Id, Type){};
/// set of type/id pairs
var types = TypeSet{};

const SubclasserSet = std.ArrayListUnmanaged(Type.Id);
/// maps type -> set of subclassers of the type
var graph = std.AutoHashMapUnmanaged(Type.Id, SubclasserSet){};

pub fn init() Allocator.Error!void {
    try prelude.init();
}

pub fn deinit() void {
    types.deinit(ally);

    var iter = map.iterator();
    while (iter.next()) |t| t.deinit(ally);
    map.deinit(ally);

    var graph_sets = graph.valueIterator();
    while (graph_sets.next()) |set| set.deinit(ally);
    graph.deinit(ally);

    prelude.deinit(ally);

    _ = gpa.deinit();

    // reset for tests
    if (builtin.is_test) {
        gpa = .{};
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
pub fn addClass(t: Type.Id, super: Type.Id) Allocator.Error!void {
    // don't create loops in the DAG
    std.debug.assert(!isCompatible(t, super));

    // get the type set for super, or create it if it doesn't exist
    const res = try graph.getOrPut(ally, super);
    if (!res.found_existing) {
        res.value_ptr.* = .{};
    }
    const set = res.value_ptr;

    try set.append(ally, t);
}

pub fn isSubclass(t: Type.Id, super: Type.Id) bool {
    const set = graph.getPtr(super) orelse {
        return false;
    };

    // check direct containment
    for (set.items) |elem| {
        if (t.eql(elem)) return true;
    }

    // check recursive containment
    for (set.items) |elem| {
        if (isSubclass(t, elem)) return true;
    }

    return false;
}

/// if inner is narrower or equal to outer
pub fn isCompatible(inner: Type.Id, outer: Type.Id) bool {
    return inner.eql(outer) or isSubclass(inner, outer);
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

fn dumpTypeImpl(t: Type, this: Type.Id) !void {
    if (builtin.mode != .Debug) {
        @compileError("don't leave this in release code");
    }

    const blox = @import("blox");
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