//! forwarded namespaces for module

pub const Ast = @import("Ast.zig");

pub const sources = @import("sources.zig");
pub const Source = sources.Source;
pub const Loc = sources.Loc;

const parser = @import("parser/parser.zig");
pub const ParseError = parser.Error;
pub const ParseFragmentInto = parser.FragmentInto;
pub const parseFragment = parser.parseFragment;
pub const parse = parser.parse;

const sema = @import("sema/sema.zig");
pub const SemaError = sema.Error;
pub const analyze = sema.analyze;

// pipes =======================================================================

const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn init() void {}

pub fn deinit(ally: Allocator) void {
    sources.deinit(ally);
}
