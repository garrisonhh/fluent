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

const Loc = union(enum) {
    reg: Jit.Register,
    /// stack offset for this value
    spilled: i32,
};

const LocMap = struct {
    const Self = @This();
    const Locations = std.AutoHashMapUnmanaged(Local, Loc);

    frame_size: u31 = 0,
    locs: Locations = .{},

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

        // TODO align this
        locmap.frame_size += @intCast(nbytes);
        const stack_index = -@as(i32, locmap.frame_size);

        const loc = Loc{ .spilled = stack_index };
        try locmap.locs.put(arena_ally, local, loc);
    }

    // must be 16-byte aligned by system v spec
    locmap.frame_size = std.mem.alignBackward(u31, locmap.frame_size, 16);

    return locmap;
}

// codegen =====================================================================

pub const Error = Jit.BuildError;

const FuncBuilderMap = std.AutoHashMapUnmanaged(Func.Ref, *Jit.BlockBuilder);
const BlockBuilderMap = std.AutoHashMapUnmanaged(Block.Ref, *Jit.BlockBuilder);

/// encode moving between locations. this will be a mov, load, store, or for
/// moving between stack locations a load and store using rax as an intermediary
fn movLoc(
    bb: *Jit.BlockBuilder,
    size: Jit.Op.Size,
    src: Loc,
    dst: Loc,
) Error!void {
    switch (src) {
        .reg => |src_reg| switch (dst) {
            .reg => |dst_reg| try bb.op(.{
                .mov = .{ .src = src_reg, .dst = dst_reg },
            }),
            .spilled => |dst_offset| try bb.op(.{
                .store = .{
                    .size = size,
                    .offset = dst_offset,
                    .src = src_reg,
                    .dst = .rbp,
                },
            }),
        },
        .spilled => |src_offset| switch (dst) {
            .reg => |dst_reg| try bb.op(.{
                .load = .{
                    .size = size,
                    .offset = src_offset,
                    .src = .rbp,
                    .dst = dst_reg,
                },
            }),
            .spilled => |dst_offset| {
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

fn assembleBlock(
    arena_ally: Allocator,
    builder: *Jit.Builder,
    fbs: FuncBuilderMap,
    bbs: BlockBuilderMap,
    bb: *Jit.BlockBuilder,
    locmap: LocMap,
    func: *const Func,
    block: *const Block,
) Error!void {
    _ = fbs;

    for (block.ops) |op| {
        const dst_loc = locmap.locs.get(op.dest).?;

        switch (op.code) {
            .constant => |val| {
                const bytes = try fluent.env.get(val).asBytes(arena_ally);

                // constant expects a little-endian u64
                const const_bytes = try arena_ally.alloc(u8, 8);
                @memset(const_bytes, 0);
                @memcpy(const_bytes[0..bytes.len], bytes);
                try bb.op(.{
                    .constant = .{ .bytes = const_bytes, .dst = .rax },
                });

                const size = Jit.Op.Size.from(bytes.len).?;
                try movLoc(bb, size, .{ .reg = .rax }, dst_loc);
            },
            .branch => |branch| {
                const cond_loc = locmap.locs.get(branch.cond).?;
                const if_true = bbs.get(branch.if_true).?.label;
                const if_false = bbs.get(branch.if_false).?.label;

                // cmp cond
                try movLoc(bb, .byte, cond_loc, .{ .reg = .rax });
                try bb.op(.{ .cmp = .{ .lhs = .rax, .rhs = .rax } });

                // branching calls
                const call_false = try builder.block();
                try bb.op(.{
                    .jump_if = .{ .cond = .z, .label = call_false.label },
                });
                try bb.op(.{ .call = if_true });
                try call_false.op(.{ .call = if_false });

                // accept return value
                const ret_t = func.locals.get(op.dest).*;
                const ret_size = Jit.Op.Size.from(typer.byteSizeOf(ret_t)).?;
                try movLoc(bb, ret_size, dst_loc, .{ .reg = .rax });
            },

            else => std.debug.panic("TODO assemble ssa {s}", .{@tagName(op.code)}),
        }
    }

    // return the block value through rax
    const ret_local = block.retLocal();
    const ret_t = func.locals.get(ret_local).*;
    const ret_loc = locmap.locs.get(ret_local).?;
    const ret_size = Jit.Op.Size.from(typer.byteSizeOf(ret_t)).?;
    try movLoc(bb, ret_size, ret_loc, .{ .reg = .rax });
    try bb.op(.ret);
}

fn assembleFunc(
    arena_ally: Allocator,
    builder: *Jit.Builder,
    fbs: FuncBuilderMap,
    entry_bb: *Jit.BlockBuilder,
    func: *const Func,
) Error!void {
    const locmap = try mapLocals(arena_ally, func);

    // map out blocks
    var bbs = BlockBuilderMap{};
    var block_iter = func.blocks.iterator();
    while (block_iter.nextEntry()) |entry| {
        const bb =
            if (entry.ref.eql(func.entry)) entry_bb else try builder.block();

        try bbs.put(arena_ally, entry.ref, bb);
    }

    // assemble all the blocks
    block_iter = func.blocks.iterator();
    while (block_iter.nextEntry()) |entry| {
        var bb = bbs.get(entry.ref).?;
        try assembleBlock(
            arena_ally,
            builder,
            fbs,
            bbs,
            bb,
            locmap,
            func,
            entry.ptr,
        );
    }
}

/// assembles ssa into the environment's jit
pub fn assemble(ally: Allocator, object: ssa.Object) Error!void {
    var arena = std.heap.ArenaAllocator.init(ally);
    defer arena.deinit();
    const arena_ally = arena.allocator();

    var builder = fluent.env.jit.builder();
    defer builder.deinit();

    // map out funcs
    var fbs = FuncBuilderMap{};
    var func_iter = object.funcs.iterator();
    while (func_iter.nextEntry()) |entry| {
        try fbs.put(arena_ally, entry.ref, try builder.block());
    }

    // assemble all the funcs
    func_iter = object.funcs.iterator();
    while (func_iter.nextEntry()) |entry| {
        const fb = fbs.get(entry.ref).?;
        try assembleFunc(arena_ally, &builder, fbs, fb, entry.ptr);
    }

    // TODO remove
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

    try builder.build();
}
