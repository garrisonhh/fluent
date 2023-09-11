const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const parser = @import("parser.zig");
const Ast = @import("Ast.zig");
const blox = @import("blox");

const TestFailure = error.TestFailure;

const ally = std.testing.allocator;

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

fn testIdent(ast: *Ast, ident: []const u8) !void {
    const expr = try ast.new(ally, .{ .ident = try ally.dupe(u8, ident) });
    try expectExpr(ast, expr, ident);
}

fn testInt(ast: *Ast, int: Ast.Expr.Int) !void {
    const expr = try ast.new(ally, .{ .int = int });

    const text = try std.fmt.allocPrint(ally, "{d}", .{int});
    defer ally.free(text);

    try expectExpr(ast, expr, text);
}

test "parsing-basic" {
    var ast = Ast{};
    defer ast.deinit(ally);

    const unit = try ast.new(ally, .unit);
    try expectExpr(&ast, unit, "()");

    try testIdent(&ast, "hello");
    try testIdent(&ast, "CamelCase");
    try testIdent(&ast, "kebab-case");

    try testInt(&ast, 0);
    try testInt(&ast, 420);
    try testInt(&ast, std.math.maxInt(Ast.Expr.Int));
}
