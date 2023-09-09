//! ast representation for fluent

const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");

pub const Node = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);

    pub const Ref = com.Ref(.ast_node, 32);
    const Map = com.RefMap(Ref, Self);


};

const Ast = @This();

map: Node.Map = .{},

pub fn deinit(self: *Ast, ally: Allocator) void {
    self.map.deinit(ally);
}