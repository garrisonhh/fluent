const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const com = @import("common");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const idents = @import("idents.zig");
const names = @import("names.zig");
const rendering = @import("rendering.zig");

pub const Ident = idents.Ident;
pub const Name = names.Name;

/// names refer to defs
pub const Def = union(enum) {
    const Self = @This();
    pub const Tag = std.meta.Tag(Self);

    pub const Value = struct {
        type: Type.Id,
        // TODO representing consts
    };

    pub const Let = struct {
        const VarMap = std.AutoHashMapUnmanaged(Ident, Type.Id);

        vars: VarMap = .{},
    };

    pub const Namespace = struct {
        const ChildSet = std.AutoHashMapUnmanaged(Name, void);

        children: ChildSet = .{},
    };

    pub const Function = struct {
        const ParamMap = std.AutoArrayHashMapUnmanaged(Ident, Type.Id);

        params: ParamMap = .{},
        returns: Type.Id,
    };

    // value: Value,
    let: Let,
    namespace: Namespace,
    function: Function,

    pub fn deinit(self: *Self, ally: Allocator) void {
        switch (self.*) {
            .let => |*let| {
                let.vars.deinit(ally);
            },
            .namespace => |*ns| {
                ns.children.deinit(ally);
            },
            .function => |*func| {
                func.params.deinit(ally);
            },
        }
    }
};

const DefMap = com.RefList(Name, Def);

var defs: DefMap = .{};

pub fn init() void {
    idents.init();
    names.init();
}

pub fn deinit(ally: Allocator) void {
    var scope_iter = defs.iterator();
    while (scope_iter.next()) |scope| scope.deinit(ally);
    defs.deinit(ally);

    names.deinit(ally);
    idents.deinit(ally);

    if (builtin.is_test) {
        defs = .{};
    }
}

/// add an entry to the env
///
/// *def ownership is moved to the env*
pub fn add(ally: Allocator, n: Name, def: Def) Allocator.Error!*Def {
    try defs.put(ally, n, def);
    return defs.get(n);
}

pub fn get(n: Name) ?*const Def {
    return getMut(n);
}

pub fn getMut(n: Name) ?*Def {
    return defs.getOpt(n);
}

// ident/name behavior =========================================================

/// intern an ident
pub const ident = idents.intern;
/// get the string backing an ident
pub const identStr = idents.str;

/// intern a name
pub const name = names.intern;
/// get the idents backing a name
pub const nameSlice = names.slice;
/// push an ident onto a name (make a child)
pub const namePush = names.push;
/// drop an ident from a name (get the parent)
pub const nameDrop = names.drop;

pub const renderIdent = rendering.renderIdent;
pub const renderName = rendering.renderName;