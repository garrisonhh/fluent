const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");
const Ast = fluent.Ast;
const typer = fluent.typer;

const TestFailure = error.TestFailure;

const ally = std.testing.allocator;

// helpers =====================================================================

fn new(ast: *Ast, init_expr: Ast.Expr) !Ast.Node {
    const loc = try fluent.sources.testLoc(ast.ally);
    return ast.new(loc, init_expr);
}

/// expects the parsed and analyzed text to match the expected node
fn expectExpr(ast: *Ast, expected: Ast.Node, text: []const u8) !void {
    const source = try fluent.sources.add(ally, "test", text);

    const node = switch (try fluent.parse(ast, source, .expr)) {
        .ok => |node| node,
        .fail => return TestFailure,
    };
    switch (try fluent.analyze(ast, node)) {
        .ok => {},
        .fail => return TestFailure,
    }

    // equality check
    const types_match = if (ast.getType(expected)) |expected_type| t: {
        const actual_type = ast.getType(node) orelse {
            break :t false;
        };

        break :t actual_type.eql(expected_type);
    } else exp_untyped: {
        break :exp_untyped ast.getType(node) == null;
    };

    const structures_match = ast.eql(expected, node);

    if (types_match and structures_match) return;

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

    const rendered_parsed = try ast.render(&mason, node);
    try mason.write(rendered_parsed, writer, .{});

    try writer.print("[expected]\n", .{});

    const rendered_expected = try ast.render(&mason, expected);
    try mason.write(rendered_expected, writer, .{});

    try bw.flush();

    return TestFailure;
}

fn testIdent(ident: []const u8) !void {
    try fluent.init(ally);
    defer fluent.deinit(ally);

    var ast = Ast.init(ally);
    defer ast.deinit();

    const expr = try new(&ast, .{
        .ident = try ally.dupe(u8, ident),
    });
    _ = try ast.setType(expr, typer.predef(.ident));

    try expectExpr(&ast, expr, ident);
}

fn testInt(int: Ast.Expr.Int) !void {
    try fluent.init(ally);
    defer fluent.deinit(ally);

    var ast = Ast.init(ally);
    defer ast.deinit();

    const expr = try new(&ast, .{ .int = int });
    _ = try ast.setType(expr, typer.predef(.i64));

    const text = try std.fmt.allocPrint(ally, "{d}", .{int});
    defer ally.free(text);

    try expectExpr(&ast, expr, text);
}

fn testReal(real: Ast.Expr.Real) !void {
    try fluent.init(ally);
    defer fluent.deinit(ally);

    var ast = Ast.init(ally);
    defer ast.deinit();

    const expr = try new(&ast, .{ .real = real });
    _ = try ast.setType(expr, typer.predef(.f64));

    const text = try std.fmt.allocPrint(ally, "{d:.16}", .{real});
    defer ally.free(text);

    try expectExpr(&ast, expr, text);
}

// parsing =====================================================================

test "parse-unit" {
    try fluent.init(ally);
    defer fluent.deinit(ally);

    var ast = Ast.init(ally);
    defer ast.deinit();

    const unit = try new(&ast, .unit);
    _ = try ast.setType(unit, typer.predef(.unit));

    try expectExpr(&ast, unit, "()");
}

test "parse-bools" {
    try fluent.init(ally);
    defer fluent.deinit(ally);

    var ast = Ast.init(ally);
    defer ast.deinit();

    const true_expr = try new(&ast, .{ .bool = true });
    const false_expr = try new(&ast, .{ .bool = false });

    _ = try ast.setType(true_expr, typer.predef(.bool));
    _ = try ast.setType(false_expr, typer.predef(.bool));

    try expectExpr(&ast, true_expr, "true");
    try expectExpr(&ast, false_expr, "false");
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