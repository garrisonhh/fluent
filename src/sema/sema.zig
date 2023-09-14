const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;

const InvalidType = error.InvalidType;
pub const SemaError = error{InvalidType};
pub const Error = Allocator.Error || SemaError;

pub fn analyze(
    ally: Allocator,
    ast: *Ast,
) Error!void {
    _ = ast;
    _ = ally;
}
