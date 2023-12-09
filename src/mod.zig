//! forwarded namespaces for module

// utilities
const scoped_map = @import("lib/scoped_map.zig");
pub const ScopedMap = scoped_map.ScopedMap;
pub const ScopedMapUnmanaged = scoped_map.ScopedMapUnmanaged;
pub const ErrorBuf = @import("lib/error_buf.zig").ErrorBuf;

// subsystems
pub const sources = @import("sources.zig");
pub const Source = sources.Source;
pub const Loc = sources.Loc;

pub const env = @import("env/env.zig");
pub const Ident = @import("env/idents.zig").Ident;
pub const Name = @import("env/names.zig").Name;
pub const Value = @import("env/value.zig").Value;

// compilation stages
pub const Ast = @import("ast/Ast.zig");

pub const Type = @import("types/type.zig").Type;
pub const typer = @import("types/typer.zig");

const parser = @import("parser/parser.zig");
pub const SyntaxErrorMeta = parser.SyntaxErrorMeta;
pub const SyntaxErrorBuf = parser.SyntaxErrorBuf;
pub const ParseError = parser.Error;
pub const parse = parser.parse;

const sema = @import("sema/sema.zig");
pub const TypeErrorMeta = sema.TypeErrorMeta;
pub const TypeErrorBuf = sema.TypeErrorBuf;
pub const SemaError = sema.Error;
pub const analyze = sema.analyze;

pub const ssa = @import("ssa/ssa.zig");

const assembler = @import("jit/assemble.zig");
pub const AssembleError = assembler.Error;
pub const assemble = assembler.assemble;

const lowering = @import("ssa/lower.zig");
pub const LowerError = lowering.Error;
pub const LowerInto = lowering.LowerInto;
pub const lower = lowering.lower;

// pipes =======================================================================

const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const options = @import("options");
const blox = @import("blox");

pub fn init() Allocator.Error!void {
    // log options if requested
    if (options.log_options) {
        const logger = std.log.scoped(.options);

        const decls = @typeInfo(options).Struct.decls;
        inline for (decls) |decl| {
            logger.info("{s}: {any}\n", .{
                decl.name,
                @field(options, decl.name),
            });
        }
    }

    // call init functions
    try typer.init();
    env.init();
}

pub fn deinit() void {
    env.deinit();
    sources.deinit();
    typer.deinit();
}

/// put this in root!
pub const std_options = struct {
    pub const log_level = switch (builtin.mode) {
        .Debug => .debug,
        .ReleaseSafe => .info,
        .ReleaseSmall, .ReleaseFast => .err,
    };

    fn logLevelColor(comptime level: std.log.Level) blox.Color {
        const c = blox.Color.init;
        return comptime switch (level) {
            .debug => c(.normal, .green),
            .info => c(.bright, .cyan),
            .warn => c(.bright, .magenta),
            .err => c(.bright, .red),
        };
    }

    fn logLevelText(comptime level: std.log.Level) []const u8 {
        return comptime switch (level) {
            .debug, .info => @tagName(level),
            .warn => "warning",
            .err => "error",
        };
    }

    fn innerLogFn(
        comptime level: std.log.Level,
        comptime scope: @TypeOf(.EnumLiteral),
        comptime format: []const u8,
        args: anytype,
    ) !void {
        const enabled = comptime switch (scope) {
            .lexer => options.log_lexer,
            else => true,
        };

        if (!enabled) return;

        // context
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        defer _ = gpa.deinit();
        const ally = gpa.allocator();

        var mason = blox.Mason.init(ally);
        defer mason.deinit();

        // render logger tag
        const tag = try mason.newBox(&.{
            try mason.newPre("[", .{}),
            try mason.newPre(@tagName(scope), .{}),
            try mason.newPre("|", .{}),
            try mason.newPre(logLevelText(level), .{
                .fg = logLevelColor(level),
            }),
            try mason.newPre("] ", .{}),
        }, .{ .direction = .right });

        // render lines of text with my blox decorators
        // this is done by intermixing std.fmt with blox in order to maintain
        // compatibility with consumer expectations
        const text = try std.fmt.allocPrint(ally, format, args);
        defer ally.free(text);

        // prune extra lines
        var slice = text;
        while (std.mem.endsWith(u8, slice, "\n")) {
            slice.len -= 1;
        }

        const is_single_line = std.mem.count(u8, slice, "\n") == 0;
        if (is_single_line) {
            const deco_fmt = mason.fmt(tag, .{ .print_final_newline = false });
            try stderr.print("{}{s}\n", .{ deco_fmt, slice });
        } else {
            // multiline
            const indent = try mason.newPre("| ", .{
                .fg = blox.Color.init(.bright, .black),
            });

            try mason.write(tag, stderr, .{});

            var lines = std.mem.splitScalar(u8, slice, '\n');
            var i: usize = 0;
            while (lines.next()) |line| : (i += 1) {
                const deco_fmt = mason.fmt(indent, .{
                    .print_final_newline = false,
                });

                try stderr.print("{}{s}\n", .{ deco_fmt, line });
            }

            try stderr.writeByte('\n');
        }
    }

    pub fn logFn(
        comptime level: std.log.Level,
        comptime scope: @TypeOf(.EnumLiteral),
        comptime format: []const u8,
        args: anytype,
    ) void {
        innerLogFn(level, scope, format, args) catch |e| {
            std.debug.panic("error in logFn: {s}\n", .{@errorName(e)});
        };
    }
};
