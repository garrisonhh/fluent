const std = @import("std");
const stderr = std.io.getStdErr().writer();
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("mod.zig");
const Ast = fluent.Ast;
const typer = fluent.typer;

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

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

    const program = switch (try fluent.parse(ast, source)) {
        .ok => |node| node,
        .fail => return TestFailure,
    };
    switch (try fluent.analyze(ast, program)) {
        .ok => {},
        .fail => return TestFailure,
    }

    if (ast.eql(expected, program)) return;

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

    const rendered_parsed = try ast.render(&mason, program);
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
    _ = try ast.setType(expr, typer.pre(.ident));

    try expectExpr(&ast, expr, ident);
}

// parsing =====================================================================

// TODO rewrite tests
