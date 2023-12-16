const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const fluent = @import("mod.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

const testFailure = error.TestFailure;
const ally = std.testing.allocator;

// end-to-end testing ==========================================================

fn succeeds(
    comptime program: []const u8,
    comptime callable: []const u8,
    comptime Output: type,
    comptime expects: Output,
) type {
    return struct {
        test "generated-end-to-end-test" {
            try fluent.init();
            defer fluent.deinit();

            const source = try fluent.sources.add("test", program);

            // parse
            var ast = fluent.Ast.init(ally);
            defer ast.deinit();

            var parse_ebuf = fluent.SyntaxErrorBuf.init(ally);
            defer parse_ebuf.deinit();

            const parse_res = fluent.parse(&ast, &parse_ebuf, source);
            const root = switch (try parse_ebuf.filter(parse_res)) {
                .payload => |x| x,
                .err => return testFailure,
            };

            // analyze
            var analyze_ebuf = fluent.TypeErrorBuf.init(ally);
            defer analyze_ebuf.deinit();

            const file = fluent.sources.get(source);
            const analyze_res = fluent.analyze(&ast, &analyze_ebuf, file.name, root);
            switch (try analyze_ebuf.filter(analyze_res)) {
                .payload => |x| x,
                .err => return testFailure,
            }

            // lower to ssa
            var ssa_object = try fluent.lower(ally, &ast, file.name, root);
            defer ssa_object.deinit(ally);

            // jit assemble ssa
            try fluent.assemble(ally, ssa_object);

            // run the function
            const func_name = try fluent.env.nameFromStr(callable);
            const Func = fn () callconv(.SysV) Output;
            const func = fluent.env.getCompiled(func_name, Func).?;
            const output = func();

            try std.testing.expectEqual(expects, output);
        }
    };
}

usingnamespace succeeds("fn f() i8 -> 42", "f", i8, 42);
usingnamespace succeeds("fn f() i16 -> 42", "f", i16, 42);
usingnamespace succeeds("fn f() i32 -> 42", "f", i32, 42);
usingnamespace succeeds("fn f() i64 -> 42", "f", i64, 42);
usingnamespace succeeds("fn f() i8 -> -42", "f", i8, -42);
usingnamespace succeeds("fn f() i16 -> -42", "f", i16, -42);
usingnamespace succeeds("fn f() i32 -> -42", "f", i32, -42);
usingnamespace succeeds("fn f() i64 -> -42", "f", i64, -42);
