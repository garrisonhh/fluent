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
pub fn addClass(
    self: *Self,
    ally: Allocator,
    t: Type.Id,
    super: Type.Id,
) Allocator.Error!void {
    try self.ensureStride(ally, @max(t.index, super.index) + 1);

    // TODO guarantee no DAG loops?

    self.setBit(t, super, 1);

    // TODO copy over super's supers
}

/// t <: super
pub fn isSubtype(self: Self, t: Type.Id, super: Type.Id) bool {
    return self.getBit(t, super) != 0;
}

/// t :> sub
pub fn isSupertype(self: Self, t: Type.Id, sub: Type.Id) bool {
    return self.getBit(sub, t) != 0;
}
