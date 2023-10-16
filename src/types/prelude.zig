//! prelude type definitions

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const typer = fluent.typer;
const env = fluent.env;

/// maps prelude type -> id
var map = std.enums.EnumMap(PreludeType, Type.Id){};

pub const PreludeType = enum {
    const Self = @This();

    /// top type
    any,
    /// bottom type
    never,

    // classes
    primitive,
    number,
    uint,
    int,
    float,

    // primitives
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
};

/// add the prelude type to typer, prelude map, and env all at once
fn put(
    ally: Allocator,
    pt: PreludeType,
    init_type: Type,
    options: typer.AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    const t = try typer.putAdvanced(ally, init_type, options);
    map.put(pt, t);

    const name = try env.nameFromStr(@tagName(pt));
    _ = try env.put(name, .{ .type = t });

    return t;
}

fn defineEmptyClass(
    ally: Allocator,
    pt: PreludeType,
    options: typer.AdvancedTypeOptions,
) Allocator.Error!void {
    const init_type = Type{
        .class = .{
            .members = try ally.alloc(Type.Class.Member, 0),
        },
    };

    _ = try put(ally, pt, init_type, options);
}

fn defineBasic(
    ally: Allocator,
    pt: PreludeType,
    init_type: Type,
    super: PreludeType,
) Allocator.Error!void {
    const t = try put(ally, pt, init_type, .{});
    try typer.addClass(ally, t, get(super));
}

fn defineNumber(
    ally: Allocator,
    pt: PreludeType,
    super: PreludeType,
) Allocator.Error!void {
    const tag = @tagName(pt);
    const bits_str = tag[1..];
    const init_type: Type = switch (tag[0]) {
        'i' => .{
            .int = .{
                .signedness = .signed,
                .bits = std.meta.stringToEnum(Type.Int.Bits, bits_str).?,
            },
        },
        'u' => .{
            .int = .{
                .signedness = .unsigned,
                .bits = std.meta.stringToEnum(Type.Int.Bits, bits_str).?,
            },
        },
        'f' => .{
            .float = .{
                .bits = std.meta.stringToEnum(Type.Float.Bits, bits_str).?,
            },
        },
        else => unreachable,
    };

    const t = try put(ally, pt, init_type, .{});
    try typer.addClass(ally, t, get(super));
}

fn dumpsubs(str: []const u8) void {
    std.debug.print("[{s}]\n", .{str});

    for (std.enums.values(PreludeType)) |pt| {
        const t = map.get(pt) orelse continue;
        std.debug.print("{s} :>", .{@tagName(pt)});

        for (std.enums.values(PreludeType)) |sub_pt| {
            const sub = map.get(sub_pt) orelse continue;
            if (typer.isSubclass(sub, t)) {
                std.debug.print(" {s}", .{@tagName(sub_pt)});
            }
        }

        std.debug.print("\n", .{});
    }

    std.debug.print("\n", .{});

    if (builtin.mode == .Debug) std.process.exit(1);
}

/// define all the prelude types
pub fn init(ally: Allocator) Allocator.Error!void {
    // any + never
    try defineEmptyClass(ally, .any, .{
        .subclasses_any = false,
        .never_subclasses = false,
    });
    try defineEmptyClass(ally, .never, .{
        .never_subclasses = false,
    });

    // primitive typeclasses
    try defineEmptyClass(ally, .primitive, .{});
    try defineEmptyClass(ally, .number, .{});
    try defineEmptyClass(ally, .uint, .{});
    try defineEmptyClass(ally, .int, .{});
    try defineEmptyClass(ally, .float, .{});
    try typer.addClass(ally, get(.number), get(.primitive));
    try typer.addClass(ally, get(.uint), get(.number));
    try typer.addClass(ally, get(.int), get(.number));
    try typer.addClass(ally, get(.float), get(.number));

    // primitives
    try defineBasic(ally, .unit, .unit, .primitive);
    try defineBasic(ally, .type, .type, .primitive);
    try defineBasic(ally, .name, .name, .primitive);
    try defineBasic(ally, .bool, .bool, .primitive);

    // number primitives
    try defineNumber(ally, .u64, .uint);
    try defineNumber(ally, .u32, .u64);
    try defineNumber(ally, .u16, .u32);
    try defineNumber(ally, .u8, .u16);

    try defineNumber(ally, .i64, .int);
    try defineNumber(ally, .i32, .i64);
    try defineNumber(ally, .i16, .i32);
    try defineNumber(ally, .i8, .i16);

    try defineNumber(ally, .f64, .float);
    try defineNumber(ally, .f32, .f64);

    // verify
    if (builtin.mode == .Debug) {
        for (std.enums.values(PreludeType)) |pt| {
            if (map.get(pt) == null) {
                std.debug.panic("forgot to define {}", .{pt});
            }

            const name = try env.nameFromStr(@tagName(pt));
            if (env.lookup(name) == null) {
                std.debug.panic("forgot to add {} to env", .{pt});
            }
        }
    }
}

pub fn deinit(ally: Allocator) void {
    _ = ally;

    // reset for tests
    if (builtin.is_test) {
        map = .{};
    }
}

pub fn get(pt: PreludeType) Type.Id {
    return map.getAssertContains(pt);
}