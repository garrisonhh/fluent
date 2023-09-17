const std = @import("std");
const Allocator = std.mem.Allocator;
const com = @import("common");
const rendering = @import("rendering.zig");
const fluent = @import("../mod.zig");
const Type = fluent.Type;

pub const Local = com.Ref(.ssa_local, 32);
pub const LocalList = com.RefList(Local, Type.Id);

/// raw values that you can insert into ssa
pub const Constant = union(enum) {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_constant, 32);
    pub const RefList = com.RefList(Ref, Self);

    unit,
    bool: bool,
    uint: u64,
    float: f64,
};

pub const Opcode = union(enum) {
    pub const Branch = struct {
        cond: Local,
        if_true: Block.Ref,
        if_false: Block.Ref,
    };

    constant: Constant.Ref,
    branch: Branch,

    add: [2]Local,
    sub: [2]Local,
    mul: [2]Local,
    div: [2]Local,
    mod: [2]Local,
};

/// instructions that operate on values
pub const Op = struct {
    dest: Local,
    code: Opcode,
};

pub const Block = struct {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_block, 32);
    pub const RefList = com.RefList(Ref, Self);

    /// the local this block receives branch values into (null for entry blocks)
    phi: ?Local,
    /// instructions for block
    ops: []const Op,
    /// what this block returns
    ret: Local,

    fn deinit(b: Block, ally: Allocator) void {
        ally.free(b.ops);
    }
};

pub const Func = struct {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_func, 32);
    pub const RefList = com.RefList(Ref, Self);

    params: []const Local,
    returns: Type.Id,
    entry: Block.Ref,
    constants: Constant.RefList,
    locals: LocalList,
    blocks: Block.RefList,

    pub fn deinit(self: *Self, ally: Allocator) void {
        ally.free(self.params);
        self.constants.deinit(ally);
        self.locals.deinit(ally);

        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |block| block.deinit(ally);
        self.blocks.deinit(ally);
    }
};

/// a mutable image of the ssa state
pub const Program = struct {
    const Self = @This();

    ally: Allocator,
    funcs: Func.RefList = .{},

    pub fn init(ally: Allocator) Self {
        return .{ .ally = ally };
    }

    pub fn deinit(self: *Self) void {
        const ally = self.ally;

        var func_iter = self.funcs.iterator();
        while (func_iter.next()) |f| f.deinit(ally);
        self.funcs.deinit(ally);
    }

    pub const RenderError = rendering.RenderError;
    pub const render = rendering.renderProgram;

    pub fn func(self: *Self) Allocator.Error!FuncBuilder {
        const ally = self.ally;

        return FuncBuilder{
            .ally = ally,
            .program = self,
            .ref = try self.funcs.new(ally),
        };
    }
};

// builders ====================================================================

pub const BlockBuilder = struct {
    const Self = @This();

    ally: Allocator,
    func: *FuncBuilder,
    ref: Block.Ref,
    phi: ?Local,
    ops: std.ArrayListUnmanaged(Op) = .{},

    /// finalize this block
    pub fn build(self: *Self, ret: Local) Allocator.Error!void {
        self.func.blocks.set(self.ref, Block{
            .phi = self.phi,
            .ops = try self.ops.toOwnedSlice(self.ally),
            .ret = ret,
        });
    }

    /// add an op to the block and returns the destination local
    pub fn op(self: *Self, t: Type.Id, code: Opcode) Allocator.Error!Local {
        const dest = try self.func.local(t);
        try self.ops.append(self.ally, Op{
            .dest = dest,
            .code = code,
        });

        return dest;
    }
};

pub const FuncBuilder = struct {
    const Self = @This();

    ally: Allocator,
    program: *Program,
    ref: Func.Ref,
    params: std.ArrayListUnmanaged(Local) = .{},
    constants: Constant.RefList = .{},
    locals: LocalList = .{},
    blocks: Block.RefList = .{},

    /// finalize this function
    pub fn build(self: *Self, entry: Block.Ref) Allocator.Error!void {
        const entry_block = self.blocks.get(entry);
        const returns = self.locals.get(entry_block.ret).*;

        self.program.funcs.set(self.ref, Func{
            .params = try self.params.toOwnedSlice(self.ally),
            .returns = returns,
            .entry = entry,
            .constants = self.constants,
            .locals = self.locals,
            .blocks = self.blocks,
        });
    }

    /// add a parameter to the function
    pub fn param(self: *Self, t: Type.Id) Allocator.Error!Local {
        const p = try self.local(t);
        try self.params.append(p);
        return p;
    }

    /// add a new constant to the function
    pub fn constant(self: *Self, c: Constant) Allocator.Error!Constant.Ref {
        return try self.constants.put(self.ally, c);
    }

    /// add a new local to the function
    pub fn local(self: *Self, t: Type.Id) Allocator.Error!Local {
        return try self.locals.put(self.ally, t);
    }

    /// add a new block to the function
    pub fn block(self: *Self, phi: ?Local) Allocator.Error!BlockBuilder {
        return BlockBuilder{
            .ally = self.ally,
            .func = self,
            .ref = try self.blocks.new(self.ally),
            .phi = phi,
        };
    }
};
