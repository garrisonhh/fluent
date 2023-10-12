//! the set of all types

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const Type = @import("type.zig").Type;
const fluent = @import("../mod.zig");

/// recursively hashes Type with awareness of self referencing
fn autoHashType(
    comptime T: type,
    x: T,
    this: Type.Id,
    hasher: *TypeSetContext.Hasher,
) void {
    const b = std.mem.asBytes;

    switch (T) {
        Type => {
            hasher.update(b(&@as(Type.Tag, x)));

            switch (x) {
                inline else => |data| {
                    autoHashType(@TypeOf(data), data, this, hasher);
                },
            }
        },

        // hash id with awareness of self
        Type.Id => {
            if (this.eql(x)) {
                // the constant used here does not matter (in terms of
                // correctness) as long as it's a constant
                hasher.update("this");
            } else {
                hasher.update(b(&x));
            }
        },

        // hash value directly
        void,
        fluent.Ident,
        fluent.Name,
        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => {
            hasher.update(b(&x));
        },

        // hash slice of hashable
        []const Type.Id => {
            for (x) |elem| {
                autoHashType(@TypeOf(elem), elem, this, hasher);
            }
        },

        // hash struct fields
        Type.Int,
        Type.Float,
        Type.Fn,
        Type.Struct,
        => {
            inline for (@typeInfo(T).Struct.fields) |field| {
                autoHashType(field.type, @field(x, field.name), this, hasher);
            }
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("cannot hash type {}", .{T});
            } else unreachable;
        },
    }
}

/// recursively checks Type for equality with awareness of self referencing

fn autoEqlType(
    comptime T: type,
    a: T,
    this_a: Type.Id,
    b: T,
    this_b: Type.Id,
) bool {
    return switch (T) {
        // trivial equalities
        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => a == b,

        // slice of autoEql-able
        []const Type.Id => eql: {
            if (a.len != b.len) {
                break :eql false;
            }

            for (a, b) |elem_a, elem_b| {
                const elems_eql = autoEqlType(
                    @TypeOf(elem_a, elem_b),
                    elem_a,
                    this_a,
                    elem_b,
                    this_b,
                );

                if (!elems_eql) break :eql false;
            }

            break :eql true;
        },

        // structs of hashable
        Type.Fn, Type.Struct => eql: {
            inline for (@typeInfo(T).Struct.fields) |field| {
                const fields_eql = autoEqlType(
                    field.type,
                    @field(a, field.name),
                    this_a,
                    @field(b, field.name),
                    this_b,
                );

                if (!fields_eql) break :eql false;
            }

            break :eql true;
        },

        Type.Id => eql: {
            const a_is_this = a.eql(this_a);
            const b_is_this = b.eql(this_b);

            // either they are both this, or neither are this and they are
            // the same id
            break :eql a_is_this and b_is_this or
                !a_is_this and !b_is_this and a.eql(b);
        },

        Type => eql: {
            if (@as(Type.Tag, a) != @as(Type.Tag, b)) {
                break :eql false;
            }

            switch (a) {
                inline else => |data_a, tag| {
                    const data_b = @field(b, @tagName(tag));
                    break :eql autoEqlType(
                        @TypeOf(data_a, data_b),
                        data_a,
                        this_a,
                        data_b,
                        this_b,
                    );
                },
            }
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("cannot auto eql two of {}", .{T});
            } else unreachable;
        },
    };
}

const TypeEntry = struct {
    id: Type.Id,
    type: *const Type,
};

const TypeSetContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xBEEF_FA75;

    pub fn hash(_: @This(), entry: TypeEntry) u64 {
        var hasher = Hasher.init(seed);
        autoHashType(Type, entry.type.*, entry.id, &hasher);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: TypeEntry, b: TypeEntry) bool {
        return autoEqlType(Type, a.type.*, a.id, b.type.*, b.id);
    }
};

const TypeSet = std.HashMapUnmanaged(
    TypeEntry,
    void,
    TypeSetContext,
    std.hash_map.default_max_load_percentage,
);

// predefs =====================================================================

/// types which are cached on init
pub const PredefinedType = enum {
    const Self = @This();

    unit,
    type,
    name,
    bool,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,

    fn initType(self: Self, ally: Allocator) Allocator.Error!Type {
        _ = ally;
        return switch (self) {
            inline .unit,
            .type,
            .name,
            .bool,
            => |tag| @unionInit(Type, @tagName(tag), {}),
            .f32 => .{ .float = .{ .bits = .@"32" } },
            .f64 => .{ .float = .{ .bits = .@"64" } },

            .u8,
            .u16,
            .u32,
            .u64,
            .i8,
            .i16,
            .i32,
            .i64,
            => t: {
                const tagname = @tagName(self);
                const signedness: Type.Int.Signedness = switch (tagname[0]) {
                    'i' => .signed,
                    'u' => .unsigned,
                    else => unreachable,
                };

                const bits = std.meta.stringToEnum(
                    Type.Int.Bits,
                    tagname[1..],
                ).?;

                break :t .{
                    .int = .{
                        .signedness = signedness,
                        .bits = bits,
                    },
                };
            },
        };
    }
};

/// a group of types cached on init
/// TODO remove this and make a Constraint type that can be used for sema expr
/// expectations
pub const PredefinedClass = enum {
    const Self = @This();

    sints,
    uints,
    ints,
    floats,
    numbers,

    fn getPredefs(self: Self) []const PredefinedType {
        return switch (self) {
            .sints => &.{ .i8, .i16, .i32, .i64 },
            .uints => &.{ .u8, .u16, .u32, .u64 },
            .ints => comptime Self.sints.getPredefs() ++
                Self.uints.getPredefs(),
            .floats => &.{ .f32, .f64 },
            .numbers => comptime Self.ints.getPredefs() ++
                Self.floats.getPredefs(),
        };
    }
};

/// ensure env is initialized before this is called
fn initPredefs(ally: Allocator) Allocator.Error!void {
    for (std.enums.values(PredefinedType)) |p| {
        const t = try p.initType(ally);
        const id = try put(ally, t);
        predef_cache.put(p, id);

        const name = try fluent.env.name(ally, &.{
            try fluent.env.ident(ally, @tagName(p)),
        });
        _ = try fluent.env.put(ally, name, .{ .type = id });
    }

    for (std.enums.values(PredefinedClass)) |c| {
        const predefs = c.getPredefs();
        const cls_types = try ally.alloc(Type.Id, predefs.len);
        for (cls_types, predefs) |*slot, p| {
            slot.* = predef(p);
        }

        predef_cls_cache.put(c, cls_types);
    }
}

// interface ===================================================================

/// maps id -> type; owns type memory
var map = com.RefMap(Type.Id, Type){};
/// set of type/id pairs
var types = TypeSet{};
var predef_cache = std.enums.EnumMap(PredefinedType, Type.Id){};
var predef_cls_cache = std.enums.EnumMap(PredefinedClass, []const Type.Id){};

pub fn init(ally: Allocator) Allocator.Error!void {
    try initPredefs(ally);
}

pub fn deinit(ally: Allocator) void {
    types.deinit(ally);

    var cls_iter = predef_cls_cache.iterator();
    while (cls_iter.next()) |entry| {
        ally.free(entry.value.*);
    }

    var iter = map.iterator();
    while (iter.next()) |t| t.deinit(ally);
    map.deinit(ally);

    // reset for tests
    if (builtin.is_test) {
        map = .{};
        types = .{};
        predef_cache = .{};
        predef_cls_cache = .{};
    }
}

pub const RenderError = rendering.RenderError;
pub const render = rendering.renderTypeId;

/// retrieve a predefined type's id
pub fn predef(p: PredefinedType) Type.Id {
    return predef_cache.getAssertContains(p);
}

/// retrieve all ids for a predefined class
pub fn predefClass(c: PredefinedClass) []const Type.Id {
    return predef_cls_cache.getAssertContains(c);
}

/// creates an invalid type id for potentially self-referential types
///
/// *you must call `setSelfRef` on the id before it is accessed*
pub fn newSelfRef(ally: Allocator) Allocator.Error!Type.Id {
    return try map.new(ally);
}

/// store a possibly self-referential type with a `this` id created using
/// `newSelfRef`, returning the intern'd type id
///
/// *type ownership is moved on calling this*
pub fn setSelfRef(
    ally: Allocator,
    this: Type.Id,
    t: Type,
) Allocator.Error!Type.Id {
    const res = try types.getOrPut(ally, .{
        .id = this,
        // remember this is a dangling pointer
        .type = &t,
    });

    if (res.found_existing) {
        // this type already exists
        t.deinit(ally);
        return res.key_ptr.id;
    }

    try map.set(ally, this, t);
    res.key_ptr.* = .{
        .id = this,
        .type = map.get(this),
    };

    return this;
}

/// create a type (convenient for types that can't be self referential)
///
/// *type ownership is moved on calling this*
pub fn put(ally: Allocator, t: Type) Allocator.Error!Type.Id {
    errdefer t.deinit(ally);
    return try setSelfRef(ally, try newSelfRef(ally), t);
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