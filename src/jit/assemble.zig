const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const typer = fluent.typer;
const ssa = fluent.ssa;
const Local = ssa.Local;
const Block = ssa.Block;
const Func = ssa.Func;
const Object = ssa.Object;
const Jit = @import("x86-jit").Jit;

// register mapping ============================================================

const RETURN_REG: Jit.Register = .rax;
const PHI_REG: Jit.Register = .rdx;

const Target = union(enum) {
    reg: Jit.Register,
    /// stack offset for this value
    stack: i32,
};

const Loc = struct {
    size: Jit.Op.Size,
    target: Target,
};

const LocMap = struct {
    const Self = @This();
    const Locations = std.AutoHashMapUnmanaged(Local, Loc);

    locs: Locations = .{},
    frame_size: u31 = 0,

    fn deinit(self: *Self, ally: Allocator) void {
        self.locs.deinit(ally);
    }
};

/// maps ssa locals to their register or stack locations
///
/// TODO currently this just places everything on the stack in sequence which is
/// incredibly stupid
fn mapLocals(
    arena_ally: Allocator,
    func: *const ssa.Func,
) Allocator.Error!LocMap {
    var locmap = LocMap{};

    var local_iter = func.locals.iterator();
    while (local_iter.nextEntry()) |entry| {
        const local = entry.ref;
        const t = entry.ptr.*;
        const nbytes = typer.byteSizeOf(t);
        const size = Jit.Op.Size.from(nbytes).?;
        // TODO actual alignment, this just gets the next power of 2
        const alignment =
            @as(u31, 1) << @intCast(@bitSizeOf(usize) - @clz(nbytes) - 1);

        locmap.frame_size += @intCast(nbytes);
        locmap.frame_size = std.mem.alignForward(
            u31,
            locmap.frame_size,
            alignment,
        );

        const stack_index = -@as(i32, locmap.frame_size);

        const loc = Loc{
            .size = size,
            .target = .{ .stack = stack_index },
        };
        try locmap.locs.put(arena_ally, local, loc);
    }

    // must be 16-byte aligned by system v spec
    locmap.frame_size = std.mem.alignForward(u31, locmap.frame_size, 16);

    return locmap;
}

// context =====================================================================

/// provides mappings between ssa funcs/blocks and builders during the jit
/// builder lifetime, and stores useful references to prevent function sig bloat
const Context = struct {
    const Self = @This();

    const SsaRef = struct {
        func: Func.Ref,
        block: Block.Ref,
    };
    const Map = std.AutoHashMapUnmanaged(SsaRef, *Jit.BlockBuilder);

    builder: *Jit.Builder,
    object: *const Object,
    map: Map = .{},

    fn init(
        arena_ally: Allocator,
        builder: *Jit.Builder,
        object: *const Object,
    ) Allocator.Error!Self {
        var self = Self{
            .builder = builder,
            .object = object,
        };

        // add all the blocks
        var func_iter = object.funcs.iterator();
        while (func_iter.nextEntry()) |func_entry| {
            const func_ref = func_entry.ref;
            const func = func_entry.ptr;

            var block_iter = func.blocks.iterator();
            while (block_iter.nextEntry()) |block_entry| {
                const block_ref = block_entry.ref;

                const key = SsaRef{
                    .func = func_ref,
                    .block = block_ref,
                };
                try self.map.put(arena_ally, key, try builder.block());
            }
        }

        return self;
    }

    fn get(self: Self, func: Func.Ref, block: Block.Ref) *Jit.BlockBuilder {
        return self.map.get(.{ .func = func, .block = block }).?;
    }

    fn typeOf(self: Self, func: Func.Ref, local: Local) fluent.Type.Id {
        const func_ptr = self.object.funcs.get(func);
        return func_ptr.locals.get(local);
    }
};

// codegen =====================================================================

pub const Error = Jit.BuildError;

/// encode moving between locations. this will be a mov, load, store, or for
/// moving between stack locations a load and store using rax as an intermediary
fn mov(
    bb: *Jit.BlockBuilder,
    size: Jit.Op.Size,
    src: Target,
    dst: Target,
) Error!void {
    switch (src) {
        .reg => |src_reg| switch (dst) {
            .reg => |dst_reg| try bb.op(.{
                .mov = .{ .src = src_reg, .dst = dst_reg },
            }),
            .stack => |dst_offset| try bb.op(.{
                .store = .{
                    .size = size,
                    .offset = dst_offset,
                    .src = src_reg,
                    .dst = .rbp,
                },
            }),
        },
        .stack => |src_offset| switch (dst) {
            .reg => |dst_reg| try bb.op(.{
                .load = .{
                    .size = size,
                    .offset = src_offset,
                    .src = .rbp,
                    .dst = dst_reg,
                },
            }),
            .stack => |dst_offset| {
                try bb.op(.{
                    .load = .{
                        .size = size,
                        .offset = src_offset,
                        .src = .rbp,
                        .dst = .rax,
                    },
                });
                try bb.op(.{
                    .store = .{
                        .size = size,
                        .offset = dst_offset,
                        .src = .rax,
                        .dst = .rbp,
                    },
                });
            },
        },
    }
}

fn movLocalTo(
    locmap: LocMap,
    bb: *Jit.BlockBuilder,
    src: Local,
    dst: Target,
) Error!void {
    const src_loc = locmap.locs.get(src).?;
    try mov(bb, src_loc.size, src_loc.target, dst);
}

fn movToLocal(
    locmap: LocMap,
    bb: *Jit.BlockBuilder,
    src: Target,
    dst: Local,
) Error!void {
    const dst_loc = locmap.locs.get(dst).?;
    try mov(bb, dst_loc.size, src, dst_loc.target);
}

fn assembleOp(
    locmap: LocMap,
    bb: *Jit.BlockBuilder,
    op: ssa.Op,
) Error!void {
    const dst_loc = locmap.locs.get(op.dest).?;

    switch (op.inst) {
        .constant => |val| {
            const bytes = try fluent.env.get(val).asBytes(bb.arena_ally);

            // constant expects a little-endian u64
            const const_bytes = try bb.arena_ally.alloc(u8, 8);
            @memset(const_bytes, 0);
            @memcpy(const_bytes[0..bytes.len], bytes);
            try bb.op(.{
                .constant = .{ .bytes = const_bytes, .dst = .rax },
            });

            const size = Jit.Op.Size.from(bytes.len).?;
            try mov(bb, size, .{ .reg = .rax }, dst_loc.target);
        },
        else => std.debug.panic("TODO assemble {s}", .{@tagName(op.inst)}),
    }
}

fn assembleBranch(
    ctx: Context,
    locmap: LocMap,
    func: Func.Ref,
    bb: *Jit.BlockBuilder,
    branch: ssa.Branch,
) Error!void {
    switch (branch) {
        .ret => |local| {
            try movLocalTo(locmap, bb, local, .{ .reg = RETURN_REG });
            try bb.op(.leave);
            try bb.op(.ret);
        },
        .jump => |jump| {
            const label = ctx.get(func, jump.to).label;

            try movLocalTo(locmap, bb, jump.phi, .{ .reg = PHI_REG });
            try bb.op(.{ .jump = label });
        },
        .branch => |br| {
            const if_true = ctx.get(func, br.if_true).label;
            const if_false = ctx.get(func, br.if_false).label;

            // cmp cond
            try movLocalTo(locmap, bb, br.cond, .{ .reg = .rax });
            try bb.op(.{ .cmp = .{ .lhs = .rax, .rhs = .rax } });

            // branch
            try bb.op(.{ .jump_if = .{ .cond = .z, .label = if_false } });
            try bb.op(.{ .jump = if_true });
        },
    }
}

fn assembleBlock(
    ctx: Context,
    locmap: LocMap,
    func_ref: Func.Ref,
    block_ref: Block.Ref,
) Error!void {
    const func = ctx.object.funcs.get(func_ref);
    const block = func.blocks.get(block_ref);
    const bb = ctx.get(func_ref, block_ref);

    if (block.phi) |phi| {
        try movToLocal(locmap, bb, .{ .reg = PHI_REG }, phi);
    }

    for (block.ops) |op| {
        try assembleOp(locmap, bb, op);
    }

    try assembleBranch(ctx, locmap, func_ref, bb, block.branch);
}

/// assembles ssa into the environment's jit
pub fn assemble(ally: Allocator, object: ssa.Object) Error!void {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    var builder = fluent.env.jit.builder();
    defer builder.deinit();

    const ctx = try Context.init(arena_ally, &builder, &object);

    // assemble everything
    var func_iter = object.funcs.iterator();
    while (func_iter.nextEntry()) |func_entry| {
        const func_ref = func_entry.ref;
        const func = func_entry.ptr;

        const locmap = try mapLocals(arena_ally, func);

        var block_iter = func.blocks.iterator();
        while (block_iter.nextEntry()) |block_entry| {
            const block_ref = block_entry.ref;
            try assembleBlock(ctx, locmap, func_ref, block_ref);
        }
    }

    try builder.build();

    // store all of the assembled functions
    func_iter = object.funcs.iterator();
    while (func_iter.nextEntry()) |entry| {
        const func_ref = entry.ref;
        const func = entry.ptr.*;
        if (func.name) |name| {
            const label = ctx.get(func_ref, func.entry).label;
            try fluent.env.addCompiled(name, label);
        }
    }

    // TODO remove or hide this behind a flag
    {
        const stderr = std.io.getStdErr().writer();
        const blox = @import("blox");

        var mason = blox.Mason.init(ally);
        defer mason.deinit();

        const rendered = builder.render(&mason) catch @panic("");

        stderr.print("[assembled]\n", .{}) catch {};
        mason.write(rendered, stderr, .{}) catch {};
        stderr.print("\n", .{}) catch {};
    }
}
