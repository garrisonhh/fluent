const std = @import("std");
const Allocator = std.mem.Allocator;
const literals = @import("literals.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const Ast = @import("Ast.zig");

const InvalidSyntax = error.InvalidSyntax;
pub const ParseError = error{InvalidSyntax};
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
        InvalidSyntax => null,
        else => e,
    };
}

fn expectToken(lexer: *Lexer, tag: Token.Tag) Error!Token {
    const token = try lexer.peek() orelse {
        return InvalidSyntax;
    };

    if (token.tag != tag) {
        return InvalidSyntax;
    }

    return token;
}

/// atom ::=
///     | ident
///     | int
///     | real
///     | `(` expr `)`
fn parseAtom(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const pk = try lexer.peek() orelse return null;

    switch (pk.tag) {
        .ident => {
            const ident = try ally.dupe(u8, lexer.slice(pk));
            lexer.accept(pk);
            return try ast.new(ally, .{ .ident = ident });
        },
        .int => {
            const text = lexer.slice(pk);
            const int = literals.parseDecimalInt(text) catch {
                return InvalidSyntax;
            };

            lexer.accept(pk);
            return try ast.new(ally, .{ .int = int });
        },
        .real => {
            const text = lexer.slice(pk);
            const real = literals.parseDecimalReal(text) catch {
                return InvalidSyntax;
            };

            lexer.accept(pk);
            return try ast.new(ally, .{ .real = real });
        },
        .lparen => {
            @panic("TODO parens");
        },
        else => {
            return null;
        },
    }
}

/// expr ::=
///     | call
///     | atom
fn parseExpr(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    // TODO parse more than atoms
    return try parseAtom(ally, ast, lexer);
}

/// def ::= `def` <expr> <expr>
fn parseDef(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const pk = try lexer.peek() orelse return null;
    if (pk.tag != .def) return ParseError.InvalidSyntax;
    lexer.accept(pk);

    const name = try parseExpr(ally, ast, lexer) orelse {
        return ParseError.InvalidSyntax;
    };
    const expr = try parseExpr(ally, ast, lexer) orelse {
        return ParseError.InvalidSyntax;
    };

    return try ast.new(ally, .{
        .def = .{
            .name = name,
            .expr = expr,
        },
    });
}

/// a program is a series of defs
fn parseProgram(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    var nodes = std.ArrayList(Ast.Node).init(ally);
    defer nodes.deinit();

    while (try parseDef(ally, ast, lexer)) |node| {
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
