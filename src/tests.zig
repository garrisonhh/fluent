const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const parser = @import("parser.zig");
const Ast = @import("Ast.zig");
const blox = @import("blox");

const TestFailure = error.TestFailure;

const ally = std.testing.allocator;

// helpers =====================================================================

fn expectExpr(ast: *Ast, expected: Ast.Node, text: []const u8) !void {
    const node = try parser.parseFragment(ally, ast, text, .expr) orelse {
        return TestFailure;
    };
    if (ast.eql(expected, node)) return;

    // nodes aren't equal :(
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    var mason = blox.Mason.init(ally);
    defer mason.deinit();

    try writer.print(
        \\[test failure in expression]
        \\{s}
        \\[parsed]
        \\
    , .{text});

    const rendered_parsed = try ast.renderNode(&mason, node);
    try mason.write(rendered_parsed, writer, .{});

    try writer.print("[expected]\n", .{});

    const rendered_expected = try ast.renderNode(&mason, expected);
    try mason.write(rendered_expected, writer, .{});

    try bw.flush();

    return TestFailure;
}

fn testIdent(ident: []const u8) !void {
    var ast = Ast{};
    defer ast.deinit(ally);

    const expr = try ast.new(ally, .{ .ident = try ally.dupe(u8, ident) });
    try expectExpr(&ast, expr, ident);
}

fn testInt(int: Ast.Expr.Int) !void {
    var ast = Ast{};
    defer ast.deinit(ally);

    const expr = try ast.new(ally, .{ .int = int });

    const text = try std.fmt.allocPrint(ally, "{d}", .{int});
    defer ally.free(text);

    try expectExpr(&ast, expr, text);
}

fn testReal(real: Ast.Expr.Real) !void {
    var ast = Ast{};
    defer ast.deinit(ally);

    const expr = try ast.new(ally, .{ .real = real });

    const text = try std.fmt.allocPrint(ally, "{d:.16}", .{real});
    defer ally.free(text);

    try expectExpr(&ast, expr, text);
}

// tests =======================================================================

test "parse-unit" {
    var ast = Ast{};
    defer ast.deinit(ally);
}

test "parse-identifiers" {
    try testIdent("hello");
    try testIdent("CamelCase");
    try testIdent("kebab-case");
}

test "parse-ints" {
    try testInt(0);
    try testInt(420);
    try testInt(std.math.maxInt(Ast.Expr.Int));
}

test "parse-reals" {
    try testReal(0.0);
    try testReal(0.0000001);
    try testReal(1231242.012232132);
}
