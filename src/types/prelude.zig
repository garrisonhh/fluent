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
    pt: PreludeType,
    init_type: Type,
    options: typer.AdvancedTypeOptions,
) Allocator.Error!Type.Id {
    const t = try typer.putAdvanced(init_type, options);
    map.put(pt, t);

    const name = try env.nameFromStr(@tagName(pt));
    _ = try env.put(name, .{ .type = t });

    return t;
}

fn defineEmptyClass(
    pt: PreludeType,
    options: typer.AdvancedTypeOptions,
) Allocator.Error!void {
    const init_type = Type{
        .class = .{ .members = &.{} },
    };

    _ = try put(pt, init_type, options);
}

fn defineBasic(
    pt: PreludeType,
    init_type: Type,
    super: PreludeType,
) Allocator.Error!void {
    const t = try put(pt, init_type, .{});
    try typer.addClass(t, get(super));
}

fn defineNumber(
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

    const t = try put(pt, init_type, .{});
    try typer.addClass(t, get(super));
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
pub fn init() Allocator.Error!void {
    // any + never
    try defineEmptyClass(.any, .{
        .subclasses_any = false,
        .never_subclasses = false,
    });
    try defineEmptyClass(.never, .{
        .never_subclasses = false,
    });

    // primitive typeclasses
    try defineEmptyClass(.primitive, .{});
    try defineEmptyClass(.number, .{});
    try defineEmptyClass(.uint, .{});
    try defineEmptyClass(.int, .{});
    try defineEmptyClass(.float, .{});
    try typer.addClass(get(.number), get(.primitive));
    try typer.addClass(get(.uint), get(.number));
    try typer.addClass(get(.int), get(.number));
    try typer.addClass(get(.float), get(.number));

    // primitives
    try defineBasic(.unit, .unit, .primitive);
    try defineBasic(.type, .type, .primitive);
    try defineBasic(.name, .name, .primitive);
    try defineBasic(.bool, .bool, .primitive);

    // number primitives
    try defineNumber(.u64, .uint);
    try defineNumber(.u32, .u64);
    try defineNumber(.u16, .u32);
    try defineNumber(.u8, .u16);

    try defineNumber(.i64, .int);
    try defineNumber(.i32, .i64);
    try defineNumber(.i16, .i32);
    try defineNumber(.i8, .i16);

    try defineNumber(.f64, .float);
    try defineNumber(.f32, .f64);

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
