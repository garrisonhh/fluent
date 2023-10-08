//! the set of all types

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const Type = @import("type.zig").Type;

/// recursively hashes Type with awareness of self referencing
fn autoHashType(
    comptime T: type,
    x: T,
    this: Type.Id,
    hasher: *TypeSetContext.Hasher,
) void {
    const b = std.mem.asBytes;

    switch (T) {
        []const u8 => hasher.update(x),

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
        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => {
            hasher.update(b(&x));
        },

        // hash slice of hashable
        []const Type.Named => {
            for (x) |elem| {
                autoHashType(@TypeOf(elem), elem, this, hasher);
            }
        },

        // hash struct fields
        Type.Int,
        Type.Float,
        Type.Named,
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
        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => a == b,

        []const u8 => std.mem.eql(u8, a, b),

        Type.Named, Type.Fn, Type.Struct => eql: {
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

/// types which are cached on init
pub const PredefinedType = enum {
    const Self = @This();

    unit,
    type,
    ident,
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
            .ident,
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

// interface ===================================================================

var map = com.RefMap(Type.Id, Type){};
var types = TypeSet{};
var predef_cache = std.enums.EnumMap(PredefinedType, Type.Id){};
var predef_cls_cache = std.enums.EnumMap(PredefinedClass, []const Type.Id){};

pub fn init(ally: Allocator) Allocator.Error!void {
    for (std.enums.values(PredefinedType)) |p| {
        const t = try p.initType(ally);
        const id = try put(ally, t);
        predef_cache.put(p, id);
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

/// creates an uninitiated type id
///
/// *you must call `set` on the id before it is accessed*
pub fn new(ally: Allocator) Allocator.Error!Type.Id {
    return try map.new(ally);
}

/// store a possibly self-referential type with a `this` id created using `new`
///
/// *type ownership is moved on calling this*
pub fn set(ally: Allocator, this: Type.Id, t: Type) Allocator.Error!void {
    const res = try types.getOrPut(ally, .{
        .id = this,
        // remember this is a dangling pointer
        .type = &t,
    });

    if (!res.found_existing) {
        // new entry
        try map.set(ally, this, t);
        res.key_ptr.* = .{
            .id = this,
            .type = map.get(this),
        };
    }
}

/// create an un-self-referential type
///
/// *type ownership is moved on calling this*
pub fn put(ally: Allocator, t: Type) Allocator.Error!Type.Id {
    errdefer t.deinit(ally);
    const id = try new(ally);
    try set(ally, id, t);

    return id;
}

pub fn get(id: Type.Id) *const Type {
    return map.get(id);
}
