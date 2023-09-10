const std = @import("std");
const Allocator = std.mem.Allocator;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const Ast = @import("Ast.zig");

pub const ParseError = error { InvalidSyntax };
pub const Error =
    Allocator.Error ||
    Lexer.Error ||
    ParseError;

/// given an error union with an error containing `error.InvalidSyntax`,
/// if `error.InvalidSyntax` is returned, returns null. otherwise provides the
/// original value.
pub fn filterSyntaxError(error_union: anytype) t: {
    const eu = @typeInfo(@TypeOf(error_union)).ErrorUnion;

    // make error set without InvalidSyntax
    var buf: []std.builtin.Type.Error = undefined;
    var i = 0;
    for (@typeInfo(eu.error_set).ErrorSet.?) |err| {
        if (!std.meta.eql(u8, err.name, @errorName(ParseError.InvalidSyntax))) {
            buf[i] = err;
            i += 1;
        }
    }

    const E = @Type(.{ .ErrorSet = &buf });
    break :t E!?eu.payload;
} {
    return error_union catch |e| switch (e) {
        ParseError.InvalidSyntax => null,
        else => e,
    };
}

fn expectToken(lexer: *Lexer, tag: Token.Tag) Error!Token {
    const token = try lexer.peek() orelse {
        return ParseError.InvalidSyntax;
    };

    if (token.tag != tag) {
        return ParseError.InvalidSyntax;
    }

    return token;
}

/// atom ::=
///     | ident
///     | int
///     | real
///     | `(` expr `)`
fn parseAtom(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    _ = ally;
    _ = ast;
    _ = lexer;
}

/// expr ::=
///     | atom
///     | call
fn parseExpr(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    _ = ally;
    _ = ast;
    _ = lexer;
}

/// def ::= `def` <ident> <expr>
fn parseDef(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    try expectToken();

    _ = ally;
    _ = ast;
    _ = lexer;
}

/// a program is a series of defs
fn parseProgram(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    var nodes = try std.ArrayList(Ast.Node).init(ally);
    defer nodes.deinit();

    while (try lexer.peek()) |_| {
        const node = try parseDef(ally, ast, lexer);
        try nodes.append(node);
    }

    return try ast.new(ally, .{
        .program = try nodes.toOwnedSlice(),
    });
}

pub fn parse(ally: Allocator, text: []const u8) Error!Ast {
    var lexer = Lexer.init(text);
    var ast = Ast{};

    ast.root = try parseProgram(ally, &ast, &lexer);

    return ast;
}