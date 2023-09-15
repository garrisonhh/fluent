const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const literals = @import("literals.zig");

pub const ParseError = error{InvalidSyntax};
pub const Error =
    Allocator.Error ||
    Lexer.Error ||
    blox.Error ||
    ParseError;

// syntax error helpers ========================================================

/// represents possible syntax errors
pub const SyntaxErrorMeta = union(enum) {
    const Self = @This();

    pub const InvalidLiteral = struct {
        loc: Loc,
        tag: Token.Tag,
    };

    pub const ExpectedOpExpr = struct {
        loc: Loc,
        /// slice from source code
        operator: []const u8,
    };

    pub const ExpectedToken = struct {
        loc: Loc,
        tag: Token.Tag,
    };

    pub const ExpectedDesc = struct {
        loc: Loc,
        desc: []const u8,
    };

    unexpected_eof: Loc,
    invalid_literal: InvalidLiteral,
    expected_op_expr: ExpectedOpExpr,
    expected_token: ExpectedToken,
    expected_desc: ExpectedDesc,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = self;
        _ = ally;
    }
};

fn invalidSyntax(ast: *Ast, meta: SyntaxErrorMeta) Error {
    try ast.addError(.{ .syntax = meta });
    return ParseError.InvalidSyntax;
}

fn errorUnexpectedEof(ast: *Ast, lexer: *const Lexer) Error {
    return invalidSyntax(ast, .{ .unexpected_eof = lexer.nextLoc() });
}

fn errorExpectedOpExpr(
    ast: *Ast,
    lexer: *const Lexer,
    operator_token: Token,
) Error {
    return invalidSyntax(ast, .{
        .expected_op_expr = .{
            .loc = operator_token.loc,
            .operator = lexer.slice(operator_token),
        },
    });
}

fn errorInvalidLiteral(ast: *Ast, token: Token) Error {
    return invalidSyntax(ast, .{
        .invalid_literal = .{
            .tag = token.tag,
            .loc = token.loc,
        },
    });
}

fn errorExpectedToken(ast: *Ast, lexer: *const Lexer, tag: Token.Tag) Error {
    return invalidSyntax(ast, .{
        .expected_token = .{
            .loc = lexer.nextLoc(),
            .tag = tag,
        },
    });
}

fn errorExpectedDesc(ast: *Ast, lexer: *const Lexer, desc: []const u8) Error {
    return invalidSyntax(ast, .{
        .expected_desc = .{
            .loc = lexer.nextLoc(),
            .desc = desc,
        },
    });
}

// parsing =====================================================================

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

fn expectToken(ast: *Ast, lexer: *Lexer, tag: Token.Tag) Error!Token {
    return try parseToken(lexer, tag) orelse
        errorExpectedToken(ast, lexer, tag);
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
                return errorExpectedOpExpr(ast, lexer, ops.getLast());
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
                    return errorExpectedOpExpr(ast, lexer, pk);
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
                return errorExpectedOpExpr(ast, lexer, pk);
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
                break :int errorInvalidLiteral(ast, pk);
            };

            lexer.accept(pk);
            break :int try ast.new(pk.loc, .{ .int = int });
        },
        .real => real: {
            const text = lexer.slice(pk);
            const real = literals.parseDecimalReal(text) catch {
                break :real errorInvalidLiteral(ast, pk);
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
                break :parens errorUnexpectedEof(ast, lexer);
            };
            if (pk2.tag == .rparen) {
                lexer.accept(pk2);
                break :parens try ast.new(pk.loc, .unit);
            }

            // wrapped expr
            const inner = try parseExpr(ast, lexer) orelse {
                const desc = "inner expression for parentheses";
                break :parens errorExpectedDesc(ast, lexer, desc);
            };
            _ = try expectToken(ast, lexer, .rparen);

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
    const let = try expectToken(ast, lexer, .let);

    const name = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "identifier for let expression");
    };

    _ = try expectToken(ast, lexer, .equals);

    const expr = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "value for let expression");
    };

    return try ast.new(let.loc, .{
        .let = .{
            .name = name,
            .expr = expr,
        },
    });
}

fn parseIf(ast: *Ast, lexer: *Lexer) Error!?Ast.Node {
    const @"if" = try expectToken(ast, lexer, .@"if");

    const cond = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "condition");
    };

    _ = try expectToken(ast, lexer, .then);

    const if_true = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "'true' branch of if/then/else");
    };

    _ = try expectToken(ast, lexer, .@"else");

    const if_false = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "'false' branch of if/then/else");
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
