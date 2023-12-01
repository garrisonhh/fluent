const std = @import("std");
const Allocator = std.mem.Allocator;
const blox = @import("blox");
const fluent = @import("../mod.zig");
const Ast = fluent.Ast;
const Loc = fluent.Loc;
const env = fluent.env;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;

pub const Error =
    Allocator.Error ||
    Lexer.Error ||
    blox.Error;

const InvalidSyntaxError = error{InvalidSyntax};
const ParseError = Error || InvalidSyntaxError;

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

    pub const ExpectedOneOf = struct {
        loc: Loc,
        tags: []const Token.Tag,
    };

    pub const ExpectedDesc = struct {
        loc: Loc,
        desc: []const u8,
    };

    unexpected_eof: Loc,
    invalid_literal: InvalidLiteral,
    expected_op_expr: ExpectedOpExpr,
    expected_token: ExpectedToken,
    expected_one_of: ExpectedOneOf,
    expected_desc: ExpectedDesc,

    pub fn deinit(self: Self, ally: Allocator) void {
        _ = self;
        _ = ally;
    }
};

fn invalidSyntax(ast: *Ast, meta: SyntaxErrorMeta) ParseError {
    try ast.addError(.{ .syntax = meta });
    return InvalidSyntaxError.InvalidSyntax;
}

fn errorUnexpectedEof(ast: *Ast, lexer: *const Lexer) ParseError {
    return invalidSyntax(ast, .{ .unexpected_eof = lexer.nextLoc() });
}

fn errorExpectedOpExpr(
    ast: *Ast,
    lexer: *const Lexer,
    operator_token: Token,
) ParseError {
    return invalidSyntax(ast, .{
        .expected_op_expr = .{
            .loc = operator_token.loc,
            .operator = lexer.slice(operator_token),
        },
    });
}

fn errorInvalidLiteral(ast: *Ast, token: Token) ParseError {
    return invalidSyntax(ast, .{
        .invalid_literal = .{
            .tag = token.tag,
            .loc = token.loc,
        },
    });
}

fn errorExpectedToken(
    ast: *Ast,
    lexer: *const Lexer,
    tag: Token.Tag,
) ParseError {
    return invalidSyntax(ast, .{
        .expected_token = .{
            .loc = lexer.nextLoc(),
            .tag = tag,
        },
    });
}

fn errorExpectedOneOf(
    ast: *Ast,
    lexer: *const Lexer,
    tags: []const Token.Tag,
) ParseError {
    return invalidSyntax(ast, .{
        .expected_one_of = .{
            .loc = lexer.nextLoc(),
            .tags = tags,
        },
    });
}

fn errorExpectedDesc(
    ast: *Ast,
    lexer: *const Lexer,
    desc: []const u8,
) ParseError {
    return invalidSyntax(ast, .{
        .expected_desc = .{
            .loc = lexer.nextLoc(),
            .desc = desc,
        },
    });
}

// parsing =====================================================================

fn parseToken(lexer: *Lexer, tag: Token.Tag) ParseError!?Token {
    const token = try lexer.peek() orelse {
        return null;
    };

    if (token.tag != tag) {
        return null;
    }

    lexer.accept(token);
    return token;
}

fn expectToken(ast: *Ast, lexer: *Lexer, tag: Token.Tag) ParseError!Token {
    return try parseToken(lexer, tag) orelse
        errorExpectedToken(ast, lexer, tag);
}

fn expectOneOf(
    ast: *Ast,
    lexer: *Lexer,
    tags: []const Token.Tag,
) ParseError!Token {
    for (tags) |tag| {
        if (try parseToken(lexer, tag)) |token| {
            return token;
        }
    }

    return errorExpectedOneOf(ast, lexer, tags);
}

const ParserFn = fn (*Ast, *Lexer) ParseError!?Ast.Node;

/// a unary prefix parser
fn unaryPrefixParser(
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return struct {
        fn prefixParser(
            ast: *Ast,
            lexer: *Lexer,
        ) ParseError!?Ast.Node {
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
        fn parser(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
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
    }.parser;
}

fn binaryRightPrecedenceParser(
    comptime valid_tags: []const Token.Tag,
    comptime inner_parser: ParserFn,
) ParserFn {
    return struct {
        fn parser(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
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
    }.parser;
}

fn binaryParser(
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
        .eq => .eq,
        else => null,
    };
}

fn parseFn(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
    const inner_parser = parsePrefixedName;

    const @"fn" = try expectToken(ast, lexer, .@"fn");

    const name = try parseAtom(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "function name");
    };

    _ = try expectToken(ast, lexer, .lparen);

    var params = std.ArrayList(Ast.Expr.KV).init(ast.ally);
    defer params.deinit();

    const pk = try lexer.peek() orelse {
        return errorUnexpectedEof(ast, lexer);
    };

    if (pk.tag == .rparen) {
        lexer.accept(pk);
    } else while (true) {
        const param_name = try parseExpr(ast, lexer) orelse {
            const desc = "parameter name";
            return errorExpectedDesc(ast, lexer, desc);
        };

        _ = try expectToken(ast, lexer, .colon);

        const param_type = try parseExpr(ast, lexer) orelse {
            const desc = "parameter type";
            return errorExpectedDesc(ast, lexer, desc);
        };

        try params.append(.{
            .key = param_name,
            .value = param_type,
        });

        const next = try expectOneOf(ast, lexer, &.{ .comma, .rparen });
        if (next.tag == .rparen) break;
    }

    const returns = try inner_parser(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "function return type");
    };

    _ = try expectToken(ast, lexer, .right_arrow);

    const body = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "function body");
    };

    return try ast.new(@"fn".loc, .{
        .@"fn" = .{
            .name = name,
            .params = try params.toOwnedSlice(),
            .returns = returns,
            .body = body,
        },
    });
}

fn parseIf(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
    const @"if" = try expectToken(ast, lexer, .@"if");

    const cond = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "condition");
    };

    _ = try expectToken(ast, lexer, .then);

    const if_true = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "'then' branch of if expression");
    };

    _ = try expectToken(ast, lexer, .@"else");

    const if_false = try parseExpr(ast, lexer) orelse {
        return errorExpectedDesc(ast, lexer, "'else' branch of if expression");
    };

    return try ast.new(@"if".loc, .{
        .@"if" = .{
            .cond = cond,
            .if_true = if_true,
            .if_false = if_false,
        },
    });
}

fn parseAtom(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
    const ally = ast.ally;
    const pk = try lexer.peek() orelse return null;
    return switch (pk.tag) {
        // tokens with one possible result
        .@"fn" => try parseFn(ast, lexer),
        .@"if" => try parseIf(ast, lexer),

        // atomic tokens
        .ident => ident: {
            const ident = try env.ident(lexer.slice(pk));

            lexer.accept(pk);
            break :ident try ast.new(pk.loc, .{ .ident = ident });
        },
        .int, .real => num: {
            const number = Ast.Expr.Number{ .str = lexer.slice(pk) };

            lexer.accept(pk);
            break :num try ast.new(pk.loc, .{ .number = number });
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

        // record literal
        .lcurly => curlys: {
            lexer.accept(pk);

            var entries = std.ArrayList(Ast.Expr.KV).init(ally);
            defer entries.deinit();

            // empty curlys
            const pk2 = try lexer.peek() orelse {
                break :curlys errorUnexpectedEof(ast, lexer);
            };
            if (pk2.tag == .rcurly) {
                lexer.accept(pk2);
                break :curlys try ast.new(pk.loc, .{
                    .record = try entries.toOwnedSlice(),
                });
            }

            // following elements
            while (true) {
                const entry_key = try parseExpr(ast, lexer) orelse {
                    const desc = "record key";
                    break :curlys errorExpectedDesc(ast, lexer, desc);
                };

                _ = try expectToken(ast, lexer, .colon);

                const entry_value = try parseExpr(ast, lexer) orelse {
                    const desc = "record value";
                    break :curlys errorExpectedDesc(ast, lexer, desc);
                };

                try entries.append(.{
                    .key = entry_key,
                    .value = entry_value,
                });

                const next = try expectOneOf(ast, lexer, &.{ .comma, .rcurly });
                if (next.tag == .rcurly) break;
            }

            break :curlys try ast.new(pk.loc, .{
                .record = try entries.toOwnedSlice(),
            });
        },

        else => null,
    };
}

const parseName = binaryParser(.left, &.{.dot}, parseAtom);

const unary_prefixes: []const Token.Tag = &.{ .minus, .ampersand };

const parsePrefixedName = unaryPrefixParser(unary_prefixes, parseName);

fn parseApplication(ast: *Ast, lexer: *Lexer) ParseError!?Ast.Node {
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

const parsePrefixedApplication = unaryPrefixParser(
    unary_prefixes,
    parseApplication,
);

const parseMulDivMod = binaryParser(
    .left,
    &.{ .star, .slash, .percent },
    parsePrefixedApplication,
);

const parseAddSub = binaryParser(
    .left,
    &.{ .plus, .minus },
    parseMulDivMod,
);

const parseConditions = binaryParser(
    .left,
    &.{.eq},
    parseAddSub,
);

const parseFuncStmt = binaryParser(
    .left,
    &.{ .right_arrow, .semicolon },
    parseConditions,
);

/// the lowest precedence parser
const parseExpr = parseFuncStmt;

/// a program is just a series of top level expressions
fn parseProgram(ast: *Ast, lexer: *Lexer) ParseError!Ast.Node {
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

pub const Result = union(enum) {
    const Self = @This();

    ok: Ast.Node,
    fail,

    fn filter(err: ParseError) Error!Self {
        return switch (err) {
            InvalidSyntaxError.InvalidSyntax => {
                return .fail;
            },
            else => |filtered| {
                return @as(Error, @errSetCast(filtered));
            },
        };
    }

    fn wrap(eu: ParseError!Ast.Node) Error!Self {
        const val = eu catch |e| {
            return filter(e);
        };

        return .{ .ok = val };
    }
};

/// parse text into an ast node
/// *remember to check ast for errors if there is a syntax error!*
pub fn parse(ast: *Ast, source: fluent.Source) Error!Result {
    var lexer = Lexer.init(source);
    return try Result.wrap(parseProgram(ast, &lexer));
}
