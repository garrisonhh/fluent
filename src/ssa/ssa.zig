const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const rendering = @import("rendering.zig");
const fluent = @import("../mod.zig");
const Type = fluent.Type;
const Name = fluent.Name;
const Value = fluent.Value;

pub const Local = com.Ref(.ssa_local, 32);
pub const LocalList = com.RefList(Local, Type.Id);

pub const Opcode = union(enum) {
    const Self = @This();

    pub const Branch = struct {
        cond: Local,
        if_true: Block.Ref,
        if_false: Block.Ref,
    };

    pub const Call = struct {
        func: Func.Ref,
        /// owned
        args: []const Local,
    };

    constant: Value.Ref,
    branch: Branch,
    call: Call,

    add: [2]Local,
    sub: [2]Local,
    mul: [2]Local,
    div: [2]Local,
    mod: [2]Local,

    eq: [2]Local,

    fn deinit(self: Self, ally: Allocator) void {
        switch (self) {
            .call => |call| ally.free(call.args),
            else => {},
        }
    }
};

/// instructions that operate on values
pub const Op = struct {
    const Self = @This();

    dest: Local,
    code: Opcode,

    fn deinit(self: Self, ally: Allocator) void {
        self.code.deinit(ally);
    }
};

pub const Block = struct {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_block, 32);
    pub const RefList = com.RefList(Ref, Self);

    ops: []const Op,

    fn deinit(self: Self, ally: Allocator) void {
        for (self.ops) |op| op.deinit(ally);
        ally.free(self.ops);
    }

    /// the local returned by this block
    pub fn retLocal(self: Self) Local {
        return self.ops[self.ops.len - 1].dest;
    }
};

pub const Func = struct {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_func, 32);
    pub const RefList = com.RefList(Ref, Self);

    name: ?Name,
    type: Type.Id,
    entry: Block.Ref,
    locals: LocalList,
    blocks: Block.RefList,

    fn deinit(self: *Self, ally: Allocator) void {
        self.locals.deinit(ally);

        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |block| block.deinit(ally);
        self.blocks.deinit(ally);
    }
};

pub const Object = struct {
    const Self = @This();

    funcs: Func.RefList,

    pub const render = rendering.renderObject;

    pub fn deinit(self: *Self, ally: Allocator) void {
        var func_iter = self.funcs.iterator();
        while (func_iter.next()) |func| func.deinit(ally);
        self.funcs.deinit(ally);
    }
};

// builders ====================================================================

pub const BlockBuilder = struct {
    const Self = @This();

    builder: *Builder,
    func: *FuncBuilder,
    ref: Block.Ref,
    /// moved to block
    ops: std.ArrayListUnmanaged(Op) = .{},

    fn errdeinit(self: *Self) void {
        self.ops.deinit(self.builder.ally);
    }

    fn build(self: *Self) Allocator.Error!Block {
        const ops = try self.ops.toOwnedSlice(self.builder.ally);
        return Block{ .ops = ops };
    }

    /// add an op to the block and returns the destination local
    ///
    /// *blocks always return the last opcode created*
    pub fn op(self: *Self, t: Type.Id, code: Opcode) Allocator.Error!Local {
        const dest = try self.func.local(t);
        try self.ops.append(self.builder.ally, Op{
            .dest = dest,
            .code = code,
        });

        return dest;
    }
};

pub const FuncBuilder = struct {
    const Self = @This();

    builder: *Builder,
    name: ?Name,
    ref: Func.Ref,
    entry: ?Block.Ref = null,
    params: std.ArrayListUnmanaged(Local) = .{},
    block_builders: std.ArrayListUnmanaged(*BlockBuilder) = .{},
    /// moved to func
    locals: LocalList = .{},
    /// moved to func
    blocks: Block.RefList = .{},

    fn errdeinit(self: *Self) void {
        for (self.block_builders.items) |bb| bb.errdeinit();

        self.locals.deinit(self.builder.ally);
        self.blocks.deinit(self.builder.ally);
    }

    fn build(self: *Self) Allocator.Error!Func {
        std.debug.assert(self.blocks.count() > 0);
        const arena_ally = self.builder.arena.allocator();

        // build all blocks
        for (self.block_builders.items) |bb| {
            self.blocks.set(bb.ref, try bb.build());
        }

        // generate type
        var params = try arena_ally.alloc(Type.Id, self.params.items.len);
        for (params, self.params.items) |*slot, param_local| {
            slot.* = self.locals.get(param_local).*;
        }

        const ret_local = self.blocks.get(self.entry.?).retLocal();
        const returns = self.locals.get(ret_local).*;

        const func_type = try fluent.typer.put(.{
            .@"fn" = .{
                .params = params,
                .returns = returns,
            },
        });

        return Func{
            .name = self.name,
            .type = func_type,
            .entry = self.entry.?,
            .locals = self.locals,
            .blocks = self.blocks,
        };
    }

    /// add a parameter to the function
    pub fn param(self: *Self, t: Type.Id) Allocator.Error!Local {
        const p = try self.local(t);
        const arena_ally = self.builder.arena.allocator();
        try self.params.append(arena_ally, p);
        return p;
    }

    /// add a new local to the function
    pub fn local(self: *Self, t: Type.Id) Allocator.Error!Local {
        return try self.locals.put(self.builder.ally, t);
    }

    /// add a new block to the function
    ///
    /// *the first block created is assumed to be the entry block*
    pub fn block(self: *Self) Allocator.Error!*BlockBuilder {
        const arena_ally = self.builder.arena.allocator();

        const ref = try self.blocks.new(self.builder.ally);
        const bb = try arena_ally.create(BlockBuilder);
        bb.* = BlockBuilder{
            .builder = self.builder,
            .func = self,
            .ref = ref,
        };

        try self.block_builders.append(arena_ally, bb);

        if (self.entry == null) {
            self.entry = ref;
        }

        return bb;
    }
};

/// builder for a linkable object
pub const Builder = struct {
    const Self = @This();

    ally: Allocator,
    arena: std.heap.ArenaAllocator,
    func_builders: std.ArrayListUnmanaged(*FuncBuilder) = .{},
    /// moved to object
    funcs: Func.RefList = .{},

    pub fn init(ally: Allocator) Self {
        return Self{
            .ally = ally,
            .arena = std.heap.ArenaAllocator.init(ally),
        };
    }

    /// errdefer this
    pub fn errdeinit(self: *Self) void {
        for (self.func_builders.items) |fb| fb.errdeinit();
        self.arena.deinit();

        self.funcs.deinit(self.ally);
    }

    /// build all functions and blocks into an object
    ///
    /// *invalidates this builder*
    pub fn build(self: *Self) Allocator.Error!Object {
        defer self.arena.deinit();

        for (self.func_builders.items) |fb| {
            self.funcs.set(fb.ref, try fb.build());
        }

        return Object{ .funcs = self.funcs };
    }

    pub fn func(self: *Self, name: ?Name) Allocator.Error!*FuncBuilder {
        const arena_ally = self.arena.allocator();

        const ref = try self.funcs.new(self.ally);
        const fb = try arena_ally.create(FuncBuilder);
        fb.* = FuncBuilder{
            .builder = self,
            .name = name,
            .ref = ref,
        };

        try self.func_builders.append(arena_ally, fb);

        return fb;
    }
};
