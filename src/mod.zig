//! forwarded namespaces for module

pub const Ast = @import("ast/Ast.zig");

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
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const options = @import("options");
const blox = @import("blox");

pub fn init() void {
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
}

pub fn deinit(ally: Allocator) void {
    sources.deinit(ally);
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

        // render text
        const text = try std.fmt.allocPrint(ally, format, args);
        defer ally.free(text);

        var slice = text;
        while (std.mem.endsWith(u8, slice, "\n")) {
            slice.len -= 1;
        }

        // render log
        const tag = try mason.newBox(&.{
            try mason.newPre("[", .{}),
            try mason.newPre(logLevelText(level), .{
                .fg = logLevelColor(level),
            }),
            try mason.newPre("|", .{}),
            try mason.newPre(@tagName(scope), .{}),
            try mason.newPre("] ", .{}),
        }, .{ .direction = .right });

        const rendered = try mason.newBox(&.{
            tag,
            try mason.newPre(slice, .{}),
        }, .{ .direction = .right });

        // write to stderr
        try mason.write(rendered, stderr, .{});
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