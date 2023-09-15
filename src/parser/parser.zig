const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const literals = @import("literals.zig");

pub const ParseError = error{InvalidSyntax};
pub const Error =
    Allocator.Error ||
    Lexer.Error ||
    blox.Error ||
    ParseError;

// TODO use blox for errors inline instead of a text desc
fn invalidSyntax(
    ast: *Ast,
    loc: ?fluent.Loc,
    desc: []const u8,
) Error {
    const mason = &ast.error_mason;

    try ast.addError(loc, try mason.newPre(desc, .{}));
    return ParseError.InvalidSyntax;
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

const ParserFn = fn (*Ast, *Lexer) Error!?Ast.Node;

/// a unary prefix parser
fn prefixPrecedenceParser(
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return struct {
        fn prefixParser(
            ast: *Ast,
            lexer: *Lexer,
        ) Error!?Ast.Node {
            // parse unary ops
            var ops = std.ArrayList(Token).init(ast.ally);
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
            var expr = try inner_parser(ast, lexer) orelse {
                if (ops.items.len == 0) {
                    // nothing was parsed
                    return null;
                }

                // an inner expression was expected
                const desc = "expected expression after unary operator";
                return invalidSyntax(ast, lexer.nextLoc(), desc);
            };

            // create unary stuff
            while (ops.popOrNull()) |op_token| {
                expr = try ast.new(op_token.loc, .{
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
        fn binaryParser(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
            var lhs = try inner_parser(ast, lexer) orelse {
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
                const rhs = try inner_parser(ast, lexer) orelse {
                    const desc = "expected expression after binary operator";
                    return invalidSyntax(ast, lexer.nextLoc(), desc);
                };

                // convert to ast node
                const op = binaryOpFromTokenTag(pk.tag).?;
                lhs = try ast.new(pk.loc, .{
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
        fn binaryParser(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
            var lhs = try inner_parser(ast, lexer) orelse {
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
            const rhs = try binaryParser(ast, lexer) orelse {
                const desc = "expected expression after binary operator";
                return invalidSyntax(ast, lexer.nextLoc(), desc);
            };

            // convert to ast node
            const op = binaryOpFromTokenTag(pk.tag).?;
            return try ast.new(pk.loc, .{
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
fn parseAtom(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const ally = ast.ally;
    const pk = try lexer.peek() orelse return null;
    return switch (pk.tag) {
        // atomic tokens
        .ident => ident: {
            const ident = try ally.dupe(u8, lexer.slice(pk));
            lexer.accept(pk);
            break :ident try ast.new(pk.loc, .{ .ident = ident });
        },
        .int => int: {
            const text = lexer.slice(pk);
            const int = literals.parseDecimalInt(text) catch {
                const desc = "invalid integer literal";
                break :int invalidSyntax(ast, pk.loc, desc);
            };

            lexer.accept(pk);
            break :int try ast.new(pk.loc, .{ .int = int });
        },
        .real => real: {
            const text = lexer.slice(pk);
            const real = literals.parseDecimalReal(text) catch {
                const desc = "invalid float literal";
                break :real invalidSyntax(ast, pk.loc, desc);
            };

            lexer.accept(pk);
            break :real try ast.new(pk.loc, .{ .real = real });
        },
        inline .true, .false => |tag| bool: {
            const value = comptime tag == .true;
            lexer.accept(pk);
            break :bool try ast.new(pk.loc, .{ .bool = value });
        },

        // parens
        .lparen => parens: {
            lexer.accept(pk);

            // unit
            const pk2 = try lexer.peek() orelse {
                const desc = "unexpected end of file";
                break :parens invalidSyntax(ast, lexer.nextLoc(), desc);
            };
            if (pk2.tag == .rparen) {
                lexer.accept(pk2);
                break :parens try ast.new(pk.loc, .unit);
            }

            // wrapped expr
            const inner = try parseExpr(ast, lexer) orelse {
                const desc = "expected inner expression for parentheses";
                break :parens invalidSyntax(ast, lexer.nextLoc(), desc);
            };
            _ = try parseToken(lexer, .rparen) orelse {
                const desc = "expected `)`";
                break :parens invalidSyntax(ast, lexer.nextLoc(), desc);
            };

            break :parens try ast.new(pk.loc, .{ .parens = inner });
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

fn parseApplication(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const inner_parser = parsePrefixedName;

    // parse inner
    const first = try inner_parser(ast, lexer) orelse {
        return null;
    };

    // try to parse second inner expr; if successful, this is an application.
    // otherwise, just return the inner parser's result.
    const second = try inner_parser(ast, lexer) orelse {
        return first;
    };

    // parse any further arguments for the application
    var app = try std.ArrayList(Ast.Node).initCapacity(ast.ally, 2);
    defer app.deinit();

    app.appendSliceAssumeCapacity(&.{ first, second });

    while (try inner_parser(ast, lexer)) |cont| {
        try app.append(cont);
    }

    return try ast.new(ast.getLoc(first), .{
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

fn parseLet(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const let = try parseToken(lexer, .let) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected `let`");
    };

    const name = try parseExpr(ast, lexer) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected decl name");
    };

    _ = try parseToken(lexer, .equals) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected `=`");
    };

    const expr = try parseExpr(ast, lexer) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected decl expression");
    };

    return try ast.new(let.loc, .{
        .let = .{
            .name = name,
            .expr = expr,
        },
    });
}

fn parseIf(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const @"if" = try parseToken(lexer, .@"if") orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected `if`");
    };

    const cond = try parseExpr(ast, lexer) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected if condition");
    };

    _ = try parseToken(lexer, .then) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected `then`");
    };

    const if_true = try parseExpr(ast, lexer) orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected branch when true");
    };

    _ = try parseToken(lexer, .@"else") orelse {
        return invalidSyntax(ast, lexer.nextLoc(), "expected `else`");
    };

    const if_false = try parseExpr(ast, lexer) orelse {
        const desc = "expected branch when false";
        return invalidSyntax(ast, lexer.nextLoc(), desc);
    };

    return try ast.new(@"if".loc, .{
        .@"if" = .{
            .cond = cond,
            .if_true = if_true,
            .if_false = if_false,
        },
    });
}

/// the lowest precedence parser
fn parseExpr(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const lowest_parser = parseStatement;

    const pk = try lexer.peek() orelse return null;
    return switch (pk.tag) {
        .let => try parseLet(ast, lexer),
        .@"if" => try parseIf(ast, lexer),

        else => try lowest_parser(ast, lexer),
    };
}

/// a program is just a series of top level expressions
fn parseProgram(ast: *Ast, lexer: *Lexer) Error!Ast.Node {
    const start_loc = lexer.nextLoc();

    var nodes = std.ArrayList(Ast.Node).init(ast.ally);
    defer nodes.deinit();

    while (try parseExpr(ast, lexer)) |node| {
        try nodes.append(node);
    }

    return try ast.new(start_loc, .{
        .program = try nodes.toOwnedSlice(),
    });
}

pub const Into = enum { program, expr };

/// parse text into an ast node
/// *remember to check ast for errors if there is a syntax error!*
pub fn parse(
    ast: *Ast,
    source: fluent.Source,
    comptime into: Into,
) Error!switch (into) {
    .program => Ast.Node,
    .expr => ?Ast.Node,
} {
    var lexer = Lexer.init(source);

    return switch (into) {
        .program => try parseProgram(ast, &lexer),
        .expr => try parseExpr(ast, &lexer),
    };
}
