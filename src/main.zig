const std = @import("std");
const Allocator = std.mem.Allocator;
const stdout = std.io.getStdOut().writer();
const stderr = std.io.getStdErr().writer();
const builtin = @import("builtin");
const kz = @import("kritzler");
const linenoize = @import("linenoize");
const Linenoise = linenoize.Linenoise;
const linenoiseEdit = linenoize.linenoiseEdit;
const util = @import("util");
const context = @import("context.zig");
const plumbing = @import("plumbing.zig");
const backend = @import("backend.zig");
const Env = backend.Env;

// this test ensures that all code is semantically analyzed
test {
    std.testing.refAllDeclsRecursive(@This());
}

/// look for a `.fluentinit` script to run on repl startup
fn runFluentInit(ally: Allocator, env: *Env) !void {
    const PATH = ".fluentinit";
    const dir = std.fs.cwd();

    dir.access(PATH, .{ .mode = .read_only }) catch return;
    try executeFile(ally, env, PATH);
}

fn repl(ally: Allocator, env: *Env) !void {
    try runFluentInit(ally, env);

    if (builtin.mode == .Debug) {
        try stdout.writeAll("[Env]\n");
        try env.dump(ally, stdout);
        try stdout.writeByte('\n');
    }

    var ln = Linenoise.init(ally);
    defer ln.deinit();

    repl: while (try ln.linenoise("> ")) |line| {
        try ln.history.add(line);

        const input = try context.addExternalSource("repl", line);

        // eval and print any messages
        const value = plumbing.exec(env, input, .expr) catch |e| {
            if (e == error.FluentError) {
                try context.flushMessages();
                continue :repl;
            } else {
                return e;
            }
        };
        defer value.deinit(ally);

        // render
        var ctx = kz.Context.init(ally);
        defer ctx.deinit();

        const tex = try value.render(&ctx, env.*);

        try ctx.write(tex, stdout);
        try stdout.writeByte('\n');
    }
}

/// the output of argument parsing
const Command = union(enum) {
    const Self = @This();
    const Tag = @typeInfo(Self).Union.tag_type.?;

    help: void,
    repl: void,
    run: []const u8,

    const Meta = struct {
        desc: []const u8,
        params: []const []const u8 = &.{},
    };

    const meta = meta: {
        var map = std.enums.EnumArray(Tag, Meta).initUndefined();
        map.set(.help, .{ .desc = "display this prompt" });
        map.set(.repl, .{ .desc = "start interactive mode" });
        map.set(.run, .{
            .desc = "run a program dynamically",
            .params = &[_][]const u8{"file"},
        });

        break :meta map;
    };

    const tags = std.ComptimeStringMap(Tag, pairs: {
        const KV = struct {
            @"0": []const u8,
            @"1": Tag
        };

        const fields = @typeInfo(Tag).Enum.fields;
        var entries: [fields.len]KV = undefined;
        for (fields) |field, i| {
            entries[i] = KV{
                .@"0" = field.name,
                .@"1" = @intToEnum(Tag, field.value),
            };
        }

        break :pairs entries;
    });
};

fn printHelp() @TypeOf(stdout).Error!void {
    try stdout.print("commands:\n\n", .{});

    for (std.enums.values(Command.Tag)) |tag| {
        const meta = Command.meta.get(tag);
        try stdout.print("{s:<16}{s}\n", .{@tagName(tag), meta.desc});
    }
}

fn executeFile(ally: Allocator, env: *Env, path: []const u8) !void {
    // required backing stuff
    const handle = context.loadSource(path) catch |e| {
        if (e == error.FileNotFound) {
            try stderr.print("could not find file at '{s}'.\n", .{path});
            return;
        } else return e;
    };

    // time execution
    const value = plumbing.exec(env, handle, .file) catch |e| {
        if (e == error.FluentError) {
            try context.flushMessages();
            return;
        } else {
            return e;
        }
    };
    defer value.deinit(env.ally);

    // render
    var ctx = kz.Context.init(ally);
    defer ctx.deinit();

    const tex = try value.render(&ctx, env.*);

    try ctx.write(tex, stdout);
}

const CommandError = error {
    BadArgs,
};

fn read_args(ally: Allocator) ![][]u8 {
    var arg_iter = try std.process.argsWithAllocator(ally);
    defer arg_iter.deinit();

    var args = std.ArrayList([]u8).init(ally);
    while (arg_iter.next()) |arg| {
        try args.append(try ally.dupe(u8, arg[0..:0]));
    }

    return args.toOwnedSlice();
}

fn parseArgs(ally: Allocator) !Command {
    const args = try read_args(ally);
    defer {
        for (args) |arg| ally.free(arg);
        ally.free(args);
    }

    if (args.len < 2) return CommandError.BadArgs;

    const tag = Command.tags.get(args[1]) orelse {
        return CommandError.BadArgs;
    };
    const meta = Command.meta.get(tag);

    if (args.len != 2 + meta.params.len) return CommandError.BadArgs;

    return switch (tag) {
        .help => Command{ .help = {} },
        .repl => Command{ .repl = {} },
        .run => Command{ .run = try ally.dupe(u8, args[2]) },
    };
}

pub fn main() !void {
    // boilerplate stuff
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .stack_trace_frames = 1000,
    }){};
    defer _ = gpa.deinit();
    // const ally = gpa.allocator();
    const ally = std.heap.page_allocator;

    try context.init(ally);
    defer context.deinit();

    var prelude = try backend.generatePrelude(ally);
    defer prelude.deinit();

    // the rest of the fucking owl
    const cmd = parseArgs(ally) catch |e| err: {
        if (e == CommandError.BadArgs) {
            break :err Command{ .help = {} };
        }

        return e;
    };

    if (builtin.mode == .Debug) {
        // display prelude as a sanity check
        try stdout.writeAll("[Prelude]\n");
        try prelude.dump(ally, stdout);
        try stdout.writeByte('\n');
    }

    switch (cmd) {
        .help => try printHelp(),
        .repl => try repl(ally, &prelude),
        .run => |path| {
            try executeFile(ally, &prelude, path);
            ally.free(path);
        }
    }
}