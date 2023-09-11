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

fn parseToken(lexer: *Lexer, tag: Token.Tag) Error!?Token {
    const token = try lexer.peek() orelse {
        return null;
    };

    if (token.tag != tag) {
        return null;
    }

    lexer.accept(token);
    return token;
}

fn unaryOpFromTokenTag(tag: Token.Tag) ?Ast.UnaryOp {
    return switch (tag) {
        .minus => .negate,
        .ampersand => .addr,
        else => null,
    };
}

fn binaryOpFromTokenTag(tag: Token.Tag) ?Ast.BinaryOp {
    return switch (tag) {
        .semicolon => .statement,
        .double_colon => .ns_access,
        .dot => .field_access,
        .plus => .add,
        .minus => .subtract,
        .star => .multiply,
        .slash => .divide,
        .percent => .modulus,
        else => null,
    };
}

const ParserFn = fn(Allocator, *Ast, *Lexer) Error!?Ast.Node;

/// a unary prefix parser
fn prefixPrecedenceParser(
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return struct {
        fn prefixParser(
            ally: Allocator,
            ast: *Ast,
            lexer: *Lexer,
        ) Error!?Ast.Node {
            // parse unary ops
            var ops = std.ArrayList(Ast.UnaryOp).init(ally);
            defer ops.deinit();

            parse: while (true) {
                // parse token
                const pk = try lexer.peek() orelse {
                    break :parse;
                };
                valid: for (valid_tags) |valid| {
                    if (pk.tag == valid) break :valid;
                } else {
                    break :parse;
                }
                lexer.accept(pk);

                const op = unaryOpFromTokenTag(pk.tag).?;
                try ops.append(op);
            }

            // parse inner
            var expr = try inner_parser(ally, ast, lexer) orelse {
                if (ops.items.len == 0) {
                    // nothing was parsed
                    return null;
                }

                // an inner expression was expected
                return InvalidSyntax;
            };

            // create unary stuff
            while (ops.popOrNull()) |op| {
                expr = try ast.new(ally, .{
                    .unary = .{
                        .op = op,
                        .child = expr,
                    },
                });
            }

            return expr;
        }
    }.prefixParser;
}

fn binaryPrecedenceParser(
    comptime binds: enum {left, right},
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return struct {
        fn binaryParser(
            ally: Allocator,
            ast: *Ast,
            lexer: *Lexer,
        ) Error!?Ast.Node {
            var lhs = try inner_parser(ally, ast, lexer) orelse {
                return null;
            };

            parse: while (true) {
                // parse token
                const pk = try lexer.peek() orelse {
                    break :parse;
                };
                valid: for (valid_tags) |valid| {
                    if (pk.tag == valid) break :valid;
                } else {
                    break :parse;
                }
                lexer.accept(pk);

                // parse rhs
                const rhs_parser = comptime switch (binds) {
                    .left => inner_parser,
                    .right => binaryParser,
                };
                const rhs = try rhs_parser(ally, ast, lexer) orelse {
                    return InvalidSyntax;
                };

                // convert to ast node
                const op = binaryOpFromTokenTag(pk.tag).?;
                lhs = try ast.new(ally, .{
                    .binary = .{
                        .op = op,
                        .lhs = lhs,
                        .rhs = rhs,
                    },
                });
            }

            return lhs;
        }
    }.binaryParser;
}

/// atom ::=
///     | ident
///     | int
///     | real
///     | `(` expr `)`
fn parseAtom(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const pk = try lexer.peek() orelse return null;
    return switch (pk.tag) {
        // atomic tokens
        .ident => ident: {
            const ident = try ally.dupe(u8, lexer.slice(pk));
            lexer.accept(pk);
            break :ident try ast.new(ally, .{ .ident = ident });
        },
        .int => int: {
            const text = lexer.slice(pk);
            const int = literals.parseDecimalInt(text) catch {
                break :int InvalidSyntax;
            };

            lexer.accept(pk);
            break :int try ast.new(ally, .{ .int = int });
        },
        .real => real: {
            const text = lexer.slice(pk);
            const real = literals.parseDecimalReal(text) catch {
                break :real InvalidSyntax;
            };

            lexer.accept(pk);
            break :real try ast.new(ally, .{ .real = real });
        },

        // parens
        .lparen => parens: {
            lexer.accept(pk);

            // unit
            const pk2 = try lexer.peek() orelse {
                break :parens InvalidSyntax;
            };
            if (pk2.tag == .rparen) {
                lexer.accept(pk2);
                break :parens try ast.new(ally, .unit);
            }

            // wrapped expr
            const inner = try parseExpr(ally, ast, lexer) orelse {
                break :parens InvalidSyntax;
            };
            _ = try parseToken(lexer, .rparen) orelse {
                break :parens InvalidSyntax;
            };

            break :parens try ast.new(ally, .{ .parens = inner });
        },

        // list/map literal
        .lcurly => {
            @panic("TODO list/map literals");
        },

        else => null,
    };
}

const parseName = binaryPrecedenceParser(
    .left,
    &.{.double_colon, .dot},
    parseAtom,
);

const unary_prefixes: []const Token.Tag = &.{.minus, .ampersand};

const parsePrefixedName = prefixPrecedenceParser(unary_prefixes, parseName);

fn parseApplication(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const inner_parser = parsePrefixedName;

    // parse inner
    const first = try inner_parser(ally, ast, lexer) orelse {
        return null;
    };

    // try to parse second inner expr; if successful, this is an application.
    // otherwise, just return the inner parser's result.
    const second = try inner_parser(ally, ast, lexer) orelse {
        return first;
    };

    // parse any further arguments for the application
    var app = try std.ArrayList(Ast.Node).initCapacity(ally, 2);
    defer app.deinit();

    app.appendSliceAssumeCapacity(&.{ first, second });

    while (try inner_parser(ally, ast, lexer)) |cont| {
        try app.append(cont);
    }

    return try ast.new(ally, .{ .call = try app.toOwnedSlice() });
}

const parsePrefixedApplication = prefixPrecedenceParser(
    unary_prefixes,
    parseApplication,
);

const parseMulDivMod = binaryPrecedenceParser(
    .left,
    &.{.star, .slash, .percent},
    parsePrefixedApplication,
);

const parseAddSub = binaryPrecedenceParser(
    .left,
    &.{.plus, .minus},
    parseMulDivMod,
);

const parseStatement = binaryPrecedenceParser(
    .left,
    &.{.semicolon},
    parseAddSub,
);

/// parseExpr is the lowest precedence parser
const parseExpr = parseStatement;

/// def ::= `def` <atom> <expr>
fn parseDef(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const pk = try lexer.peek() orelse return null;
    if (pk.tag != .def) {
        std.debug.print("expected def got {}\n", .{pk.tag});
        return InvalidSyntax;
    }
    lexer.accept(pk);

    const name = try parseAtom(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };
    const expr = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
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

    return try ast.new(ally, .{ .program = try nodes.toOwnedSlice() });
}

pub fn parse(ally: Allocator, text: []const u8) Error!Ast {
    var lexer = Lexer.init(text);
    var ast = Ast{};
    errdefer ast.deinit(ally);

    ast.root = try parseProgram(ally, &ast, &lexer);

    return ast;
}
