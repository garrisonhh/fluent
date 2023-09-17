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

    bool: bool,
    uint: u64,
    float: f64,
};

pub const Opcode = union(enum) {
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

/// instructions that determine control flow between blocks
pub const Branch = union(enum) {
    pub const Return = struct {
        value: Local,
    };

    pub const Jump = struct {
        value: Local,
        dest: Block.Ref,
    };

    /// return from the function
    @"return": Return,
    /// unconditional jump to another block
    jump: Jump,
};

pub const Block = struct {
    const Self = @This();
    pub const Ref = com.Ref(.ssa_block, 32);
    pub const RefList = com.RefList(Ref, Self);

    /// the local this block receives branch values into
    phi: ?Local,
    /// instructions for block
    ops: []const Op,
    /// where this block goes after executing
    branch: Branch,

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
    locals: LocalList,
    blocks: Block.RefList,

    fn deinit(f: *Func, ally: Allocator) void {
        ally.free(f.params);
        f.locals.deinit(ally);

        var block_iter = f.blocks.iterator();
        while (block_iter.next()) |block| block.deinit(ally);
        f.blocks.deinit(ally);
    }
};

pub const Program = struct {
    constants: Constant.RefList,
    funcs: Func.RefList,

    pub fn deinit(p: *Program, ally: Allocator) void {
        p.constants.deinit(ally);

        var func_iter = p.funcs.iterator();
        while (func_iter.next()) |func| func.deinit(ally);
        p.funcs.deinit(ally);
    }

    pub const RenderError = rendering.RenderError;
    pub const render = rendering.renderProgram;
};

// builders ====================================================================

pub const BlockBuilder = struct {
    const Self = @This();

    ally: Allocator,
    func: *FuncBuilder,
    ref: Block.Ref,
    phi: Local,
    ops: std.ArrayListUnmanaged(Op) = .{},

    /// finalize this block
    pub fn build(self: *Self, branch: Branch) Allocator.Error!void {
        self.func.blocks.set(self.ref, Block{
            .phi = self.phi,
            .ops = try self.ops.toOwnedSlice(self.ally),
            .branch = branch,
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
    program: *Builder,
    ref: Func.Ref,
    params: std.ArrayListUnmanaged(Local) = .{},
    locals: LocalList = .{},
    blocks: Block.RefList = .{},

    /// finalize this function
    pub fn build(self: *Self, returns: Type.Id) Allocator.Error!void {
        self.program.funcs.set(self.ref, Func{
            .params = try self.params.toOwnedSlice(self.ally),
            .returns = returns,
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

    /// add a new local to the function
    pub fn local(self: *Self, t: Type.Id) Allocator.Error!Local {
        return try self.locals.put(self.ally, t);
    }

    /// add a new block to the function
    pub fn block(self: *Self, phi: ?Local) Allocator.Error!BlockBuilder {
        return BlockBuilder{
            .ally = self.ally,
            .parent = self,
            .ref = try self.blocks.new(),
            .phi = phi,
        };
    }
};

/// builds programs. the abstraction for creating ssa.
pub const Builder = struct {
    const Self = @This();

    ally: Allocator,
    constants: Constant.RefList = .{},
    funcs: Func.RefList = .{},

    pub fn init(ally: Allocator) Self {
        return .{ .ally = ally };
    }

    /// finalize the program
    pub fn build(self: Self) Program {
        return Program{
            .constants = self.constants,
            .funcs = self.funcs,
        };
    }

    /// add a new constant to the function
    pub fn constant(self: *Self, t: Type.Id) Allocator.Error!Constant.Ref {
        return try self.constants.put(self.ally, t);
    }

    /// add a new function to the program
    pub fn func(self: *Self) Allocator.Error!FuncBuilder {
        return FuncBuilder{
            .ally = self.ally,
            .parent = self,
            .ref = try self.funcs.new(),
        };
    }
};
