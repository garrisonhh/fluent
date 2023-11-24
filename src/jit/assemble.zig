const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const env = fluent.env;
const ssa = fluent.ssa;
const Func = ssa.Func;
const Jit = @import("x86-jit").Jit;

const AssembledMap = std.AutoHashMap(Func.Ref, Jit.Label);

pub const Error = Jit.BuildError;

/// assembles ssa into the jit
pub fn assemble(ally: Allocator, ssa_builder: *const ssa.Builder) Error!void {
    var map = AssembledMap.init(ally);
    defer map.deinit();

    _ = ssa_builder;
    @panic("TODO");
}