//! a data structure representing a DAG for types and supertypes

const std = @import("std");
const Allocator = std.mem.Allocator;
const Type = @import("type.zig").Type;

const Self = @This();

/// should be a power of 2
const initial_stride = 16;

mem: []u8,
/// stride of row (in bytes)
stride: usize,

pub fn init(ally: Allocator) Allocator.Error!Self {
    var self = Self{
        .mem = try ally.alloc(u8, 0),
        .stride = 0,
    };

    try self.ensureStride(ally, initial_stride);

    return self;
}

pub fn deinit(self: Self, ally: Allocator) void {
    ally.free(self.mem);
}

fn ensureStride(
    self: *Self,
    ally: Allocator,
    required: usize,
) Allocator.Error!void {
    // check for expansion required
    var next_stride = if (self.stride == 0) initial_stride else self.stride;
    while (next_stride < required) {
        next_stride *= 2;
    }

    if (next_stride == self.stride) return;

    // expand to fit next stride
    const next_mem = try ally.alloc(u8, next_stride * next_stride);
    @memset(next_mem, 0);

    if (self.stride > 0) {
        var src_windows =
            std.mem.window(u8, self.mem, self.stride, self.stride);
        var i: usize = 0;
        while (src_windows.next()) |src| : (i += next_stride) {
            const dst = next_mem[i .. i + self.stride];
            @memcpy(dst, src);
        }
    }

    ally.free(self.mem);
    self.mem = next_mem;
    self.stride = next_stride;
}

fn getBit(self: Self, t: Type.Id, super: Type.Id) u1 {
    const x: usize = t.index / 8;
    const y: usize = super.index;
    const index = x + y * self.stride;

    if (index >= self.mem.len) {
        return 0;
    }

    const bit_offset: u3 = @intCast(t.index % 8);
    return @truncate(self.mem[index] >> bit_offset);
}

fn setBit(self: Self, t: Type.Id, super: Type.Id, value: u1) void {
    const x: usize = t.index / 8;
    const y: usize = super.index;
    const index = x + y * self.stride;

    const bit_index: u3 = @intCast(t.index % 8);
    switch (value) {
        0 => self.mem[index] &= ~std.math.shl(u8, value, bit_index),
        1 => self.mem[index] |= std.math.shl(u8, value, bit_index),
    }
}

/// mark t as a subclass of super
pub fn addSupertype(
    self: *Self,
    ally: Allocator,
    t: Type.Id,
    super: Type.Id,
) Allocator.Error!void {
    try self.ensureStride(ally, @max(t.index, super.index) + 1);

    std.debug.assert(!self.isSubtype(t, super));
    self.setBit(t, super, 1);

    var super_supers = self.supertypes(super);
    while (super_supers.next()) |super_super| {
        self.setBit(t, super_super, 1);
    }
}

/// does `t <: super`
pub fn isSubtype(self: Self, t: Type.Id, super: Type.Id) bool {
    return self.getBit(t, super) != 0;
}

/// does `t :> sub`
pub fn isSupertype(self: Self, t: Type.Id, sub: Type.Id) bool {
    return self.getBit(sub, t) != 0;
}

pub fn subtypes(self: *const Self, t: Type.Id) Iterator(.sub) {
    return .{ .graph = self, .t = t };
}

pub fn supertypes(self: *const Self, t: Type.Id) Iterator(.super) {
    return .{ .graph = self, .t = t };
}

pub const IteratorKind = enum { sub, super };
pub fn Iterator(comptime kind: IteratorKind) type {
    return struct {
        graph: *const Self,
        t: Type.Id,
        other: Type.Id = .{ .index = 0 },

        pub fn next(iter: *@This()) ?Type.Id {
            while (true) {
                if (iter.other.index >= iter.graph.stride * 8) {
                    return null;
                }

                const should_yield = switch (kind) {
                    .sub => iter.graph.isSupertype(iter.t, iter.other),
                    .super => iter.graph.isSubtype(iter.t, iter.other),
                };

                defer iter.other.index += 1;
                if (should_yield) return iter.other;
            }
        }
    };
}