//! canonical data representation for the bytecode model

const std = @import("std");
const builtin = @import("builtin");

pub const Number = @import("canon/number.zig");
pub const Builtin = @import("canon/builtins.zig").Builtin;
pub const Value = @import("canon/value.zig");
pub usingnamespace @import("canon/prelude.zig");
pub usingnamespace @import("canon/fluency.zig");

/// given up to 8 bytes, return canonical u64 representation for vm
pub fn to(bytes: []const u8) u64 {
    std.debug.assert(bytes.len <= 8);
    std.debug.assert(builtin.cpu.arch.endian() == .Little);

    var dst: [8]u8 align(8) = undefined;
    std.mem.set(u8, &dst, 0);
    std.mem.copy(u8, &dst, bytes);

    return @ptrCast(*const u64, &dst).*;
}

pub fn from(n: *const u64) []const u8 {
    const nbytes = 8 - (@ctz(n.*) / 8);
    return @ptrCast(*const [8]u8, n)[0..nbytes];
}

pub fn intoValue(n: *u64) Value {
    const nbytes = 8 - (@ctz(n.*) / 8);
    const buf = @ptrCast(*[8]u8, n)[0..nbytes];

    return Value.of(buf);
}