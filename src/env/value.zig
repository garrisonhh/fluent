const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const typer = fluent.typer;
const Ident = @import("idents.zig").Ident;
const Name = @import("names.zig").Name;

/// raw values used to represent fluent data across the compiler
pub const Value = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);
    pub const Ref = com.Ref(.value, 64);
    pub const RefList = com.RefList(Ref, Self);

    pub const UInt = union(enum) {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
    };

    pub const Int = union(enum) {
        i8: i8,
        i16: i16,
        i32: i32,
        i64: i64,
    };

    pub const Float = union(enum) {
        f32: f32,
        f64: f64,
    };

    pub const Function = struct {
        const ParamMap = std.AutoArrayHashMapUnmanaged(Ident, Type.Id);

        type: Type.Id,
        params: ParamMap = .{},
        /// null if this function hasn't yet been compiled
        ssa: ?fluent.ssa.Func.Ref,
    };

    unit,
    name: Name,
    type: Type.Id,
    bool: bool,
    uint: UInt,
    int: Int,
    float: Float,
    function: Function,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = ally;
        switch (self) {
            else => {},
        }
    }

    /// determines the type based on the data format
    /// TODO maybe start caching this in env when it gets complicated
    pub fn findType(self: Self) Type.Id {
        return switch (self) {
            inline .unit, .name, .type, .bool => |_, tag| predef: {
                const p = std.enums.nameCast(typer.PredefinedType, tag);
                break :predef typer.predef(p);
            },
            inline .uint, .int, .float => |num| switch (num) {
                inline else => |_, tag| predef: {
                    const p = std.enums.nameCast(typer.PredefinedType, tag);
                    break :predef typer.predef(p);
                },
            },
            .function => |f| f.type,
        };
    }

    pub fn clone(self: Self, ally: Allocator) Allocator.Error!Self {
        _ = self;
        _ = ally;
        @compileError("TODO");
    }

    pub fn eql(self: Self, other: Self) bool {
        if (@as(Tag, self) != @as(Tag, other)) {
            return false;
        }

        return switch (self) {
            .unit, .bool, .uint, .float => std.meta.eql(self, other),

            inline .name,
            .type,
            => |ref, tag| ref.eql(@field(other, @tagName(tag))),

            else => |tag| std.debug.panic(
                "TODO value.eql for {s}",
                .{@tagName(tag)},
            ),
        };
    }
};
