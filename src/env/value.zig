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

    pub const FnDef = struct {
        pub const ParamMap = std.AutoArrayHashMapUnmanaged(Ident, Type.Id);

        type: Type.Id,
        params: ParamMap,
    };

    unit,
    bool: bool,
    uint: UInt,
    int: Int,
    float: Float,

    // quoted only
    type: Type.Id,
    name: Name,
    fn_def: FnDef,

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .unit, .name, .type, .bool, .uint, .int, .float => {},
            .fn_def => |*fd| {
                fd.params.deinit(ally);
            },
        }
    }

    /// determines the type based on the data format
    pub fn findType(self: Self) Type.Id {
        // NOTE make sure this stays relatively trivial. for complex types,
        // precomputing and storing the type information inside the value makes
        // sense. or some other caching method.
        return switch (self) {
            inline .unit, .name, .type, .bool => |_, tag| predef: {
                const pt = std.enums.nameCast(typer.PreludeType, tag);
                break :predef typer.pre(pt);
            },
            inline .uint, .int, .float => |num| switch (num) {
                inline else => |_, tag| predef: {
                    const pt = std.enums.nameCast(typer.PreludeType, tag);
                    break :predef typer.pre(pt);
                },
            },
            .fn_def => |fd| fd.type,
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
