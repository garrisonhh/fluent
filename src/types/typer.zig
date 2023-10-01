//! the set of all types

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const rendering = @import("rendering.zig");
const Type = @import("type.zig").Type;

const TypeHashMapContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xBEEF_FA75;

    pub fn hash(_: @This(), t: *const Type) u64 {
        var hasher = Hasher.init(seed);
        std.hash.autoHash(&hasher, t.*);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: *const Type, b: *const Type) bool {
        return a.eql(b.*);
    }
};

const TypeHashMap = std.HashMapUnmanaged(
    *const Type,
    Type.Id,
    TypeHashMapContext,
    std.hash_map.default_max_load_percentage,
);

/// types which are cached on init
pub const PredefinedType = enum {
    const Self = @This();

    unit,
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

    fn initType(self: Self) Type {
        return switch (self) {
            inline .unit,
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
var types = TypeHashMap{};
var predef_cache = std.enums.EnumMap(PredefinedType, Type.Id){};
var predef_cls_cache = std.enums.EnumMap(PredefinedClass, []const Type.Id){};

pub fn init(ally: Allocator) Allocator.Error!void {
    for (std.enums.values(PredefinedType)) |p| {
        const id = try put(ally, p.initType());
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

/// returns the unique id for this type, which may be newly generated or
/// retrieved from a previous put() call
pub fn put(ally: Allocator, init_type: Type) Allocator.Error!Type.Id {
    // find previous id
    const res = try types.getOrPut(ally, &init_type);
    if (res.found_existing) return res.value_ptr.*;

    // new id
    const id = try map.put(ally, init_type);
    res.key_ptr.* = map.get(id);
    res.value_ptr.* = id;

    return id;
}

pub fn get(id: Type.Id) *const Type {
    return map.get(id);
}
