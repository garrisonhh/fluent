const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const literals = @import("literals.zig");

const InvalidSyntax = error.InvalidSyntax;
pub const ParseError = error{InvalidSyntax};
pub const Error =
    Allocator.Error ||
    Lexer.Error ||
    ParseError;

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

const ParserFn = fn (Allocator, *Ast, *Lexer) Error!?Ast.Node;

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
            var ops = std.ArrayList(Token).init(ally);
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

                try ops.append(pk);
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
            while (ops.popOrNull()) |op_token| {
                expr = try ast.new(ally, op_token.loc, .{
                    .unary = .{
                        .op = unaryOpFromTokenTag(op_token.tag).?,
                        .child = expr,
                    },
                });
            }

            return expr;
        }
    }.prefixParser;
}

fn binaryLeftPrecedenceParser(
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

                for (valid_tags) |valid| {
                    if (pk.tag == valid) break;
                } else break :parse;
                lexer.accept(pk);

                // parse rhs
                const rhs = try inner_parser(ally, ast, lexer) orelse {
                    return InvalidSyntax;
                };

                // convert to ast node
                const op = binaryOpFromTokenTag(pk.tag).?;
                lhs = try ast.new(ally, pk.loc, .{
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

fn binaryRightPrecedenceParser(
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

            // parse token
            const pk = try lexer.peek() orelse {
                return lhs;
            };
            for (valid_tags) |valid| {
                if (pk.tag == valid) break;
            } else {
                return lhs;
            }
            lexer.accept(pk);

            // parse rhs
            const rhs = try binaryParser(ally, ast, lexer) orelse {
                return InvalidSyntax;
            };

            // convert to ast node
            const op = binaryOpFromTokenTag(pk.tag).?;
            return try ast.new(ally, pk.loc, .{
                .binary = .{
                    .op = op,
                    .lhs = lhs,
                    .rhs = rhs,
                },
            });
        }
    }.binaryParser;
}

fn binaryPrecedenceParser(
    comptime binds: enum { left, right },
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return switch (binds) {
        .left => binaryLeftPrecedenceParser(valid_tags, inner_parser),
        .right => binaryRightPrecedenceParser(valid_tags, inner_parser),
    };
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
        .dot => .field_access,
        .plus => .add,
        .minus => .subtract,
        .star => .multiply,
        .slash => .divide,
        .percent => .modulus,
        else => null,
    };
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
            break :ident try ast.new(ally, pk.loc, .{ .ident = ident });
        },
        .int => int: {
            const text = lexer.slice(pk);
            const int = literals.parseDecimalInt(text) catch {
                break :int InvalidSyntax;
            };

            lexer.accept(pk);
            break :int try ast.new(ally, pk.loc, .{ .int = int });
        },
        .real => real: {
            const text = lexer.slice(pk);
            const real = literals.parseDecimalReal(text) catch {
                break :real InvalidSyntax;
            };

            lexer.accept(pk);
            break :real try ast.new(ally, pk.loc, .{ .real = real });
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
                break :parens try ast.new(ally, pk.loc, .unit);
            }

            // wrapped expr
            const inner = try parseExpr(ally, ast, lexer) orelse {
                break :parens InvalidSyntax;
            };
            _ = try parseToken(lexer, .rparen) orelse {
                break :parens InvalidSyntax;
            };

            break :parens try ast.new(ally, pk.loc, .{ .parens = inner });
        },

        // list/map literal
        .lcurly => {
            @panic("TODO list/map literals");
        },

        else => null,
    };
}

const parseName = binaryPrecedenceParser(.left, &.{.dot}, parseAtom);

const unary_prefixes: []const Token.Tag = &.{ .minus, .ampersand };

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

    return try ast.new(ally, ast.getLoc(first), .{
        .call = try app.toOwnedSlice(),
    });
}

const parsePrefixedApplication = prefixPrecedenceParser(
    unary_prefixes,
    parseApplication,
);

const parseMulDivMod = binaryPrecedenceParser(
    .left,
    &.{ .star, .slash, .percent },
    parsePrefixedApplication,
);

const parseAddSub = binaryPrecedenceParser(
    .left,
    &.{ .plus, .minus },
    parseMulDivMod,
);

const parseStatement = binaryPrecedenceParser(
    .left,
    &.{.semicolon},
    parseAddSub,
);

const parseDef = binaryPrecedenceParser(
    .right,
    &.{.double_colon},
    parseStatement,
);

fn parseLet(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const let = try parseToken(lexer, .let) orelse {
        return InvalidSyntax;
    };

    const name = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };

    _ = try parseToken(lexer, .equals) orelse {
        return InvalidSyntax;
    };

    const expr = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };

    return try ast.new(ally, let.loc, .{
        .let = .{
            .name = name,
            .expr = expr,
        },
    });
}

fn parseIf(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const @"if" = try parseToken(lexer, .@"if") orelse {
        return InvalidSyntax;
    };

    const cond = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };

    _ = try parseToken(lexer, .then) orelse {
        return InvalidSyntax;
    };

    const if_true = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };

    _ = try parseToken(lexer, .@"else") orelse {
        return InvalidSyntax;
    };

    const if_false = try parseExpr(ally, ast, lexer) orelse {
        return InvalidSyntax;
    };

    return try ast.new(ally, @"if".loc, .{
        .@"if" = .{
            .cond = cond,
            .if_true = if_true,
            .if_false = if_false,
        },
    });
}

/// the lowest precedence parser
fn parseExpr(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const lowest_parser = parseStatement;

    const pk = try lexer.peek() orelse return null;
    return switch (pk.tag) {
        .let => try parseLet(ally, ast, lexer),
        .@"if" => try parseIf(ally, ast, lexer),

        else => try lowest_parser(ally, ast, lexer),
    };
}

/// a program is just a series of top level expressions
fn parseProgram(ally: Allocator, ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    const start_loc = lexer.loc;

    var nodes = std.ArrayList(Ast.Node).init(ally);
    defer nodes.deinit();

    while (try parseExpr(ally, ast, lexer)) |node| {
        try nodes.append(node);
    }

    return try ast.new(ally, start_loc, .{
        .program = try nodes.toOwnedSlice(),
    });
}

pub const FragmentInto = enum { program, expr };

/// parse text into an unattached node with an ast as the context
pub fn parseFragment(
    ally: Allocator,
    ast: *Ast,
    source: fluent.Source,
    comptime into: FragmentInto,
) Error!?Ast.Node {
    var lexer = Lexer.init(source);

    const parser = switch (into) {
        .program => parseProgram,
        .expr => parseExpr,
    };

    return try parser(ally, ast, &lexer);
}

/// parse text into an ast
pub fn parse(ally: Allocator, source: fluent.Source) Error!Ast {
    var ast = Ast{};
    errdefer ast.deinit(ally);

    ast.root = try parseFragment(ally, &ast, source, .program);

    return ast;
}
