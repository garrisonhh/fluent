const std = @import("std");
const options = @import("options");
const com = @import("common");
const Codepoint = com.utf8.Codepoint;
const fluent = @import("../mod.zig");
const Loc = fluent.Loc;
const blox = @import("blox");

const logger = std.log.scoped(.lexer);

const LexError = error{InvalidInput};
pub const Error =
    Codepoint.ParseError ||
    LexError;

pub const Token = struct {
    const Self = @This();

    pub const Tag = enum {
        ident,
        int,
        real,

        let,
        @"if",
        then,
        @"else",

        lparen,
        rparen,
        lcurly,
        rcurly,

        ampersand,
        equals,
        colon,
        semicolon,
        dot,
        comma,
        minus,
        plus,
        star,
        slash,
        percent,
    };

    /// starting location
    loc: Loc,
    tag: Tag,
    start: usize,
    stop: usize,

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("<{} {s}>", .{self.loc, @tagName(self.tag)});
    }
};

const Lexer = @This();

const CodepointCache = com.BoundedRingBuffer(Codepoint, 8);
const TokenCache = com.BoundedRingBuffer(Token, 8);

/// the location of the peeked/next token
loc: Loc,
iter: Codepoint.Iterator,
cache: TokenCache = .{},

pub fn init(source: fluent.Source) Lexer {
    const text = fluent.sources.get(source).text;
    return .{
        .loc = .{
            .source = source,
            .line_index = 0,
            .char_index = 0,
        },
        .iter = Codepoint.parse(text),
    };
}

pub fn slice(self: Lexer, tok: Token) []const u8 {
    return self.iter.text[tok.start..tok.stop];
}

// forwarded codepoint iterator ================================================

fn index(self: Lexer) u32 {
    return @intCast(self.iter.byte_index);
}

fn peekC(self: *Lexer) Error!?Codepoint {
    return self.iter.peek();
}

fn acceptC(self: *Lexer, c: Codepoint) void {
    self.loc.char_index += 1;
    if (c.eql(Codepoint.ct("\n"))) {
        self.loc.line_index += 1;
        self.loc.char_index = 0;
    }

    self.iter.accept(c);
}

fn peekSlice(self: *Lexer, buf: []Codepoint, n: usize) Error![]const Codepoint {
    return try self.iter.peekSlice(buf, n);
}

fn acceptSlice(self: *Lexer, str: []const Codepoint) void {
    for (str) |c| self.acceptC(c);
}

// codepoint classification ====================================================

fn isIdentStart(c: Codepoint) bool {
    return c.isAlpha();
}

fn isIdentTail(c: Codepoint) bool {
    return c.isAlpha() or c.c == '-' or c.isDigit(10);
}

fn isDecimal(c: Codepoint) bool {
    return c.isDigit(10);
}

// tokenization ================================================================

/// keywords followw the same rules as idents
const Keyword = struct {
    const Self = @This();

    str: []const u8,
    tag: Token.Tag,

    fn make(str: []const u8, tag: Token.Tag) Self {
        return .{ .str = str, .tag = tag };
    }

    const list = [_]Keyword{
        make("let", .let),
        make("if", .@"if"),
        make("then", .then),
        make("else", .@"else"),
    };
};

/// symbols don't require whitespace separation
const Symbol = struct {
    const Self = @This();

    str: []const Codepoint,
    tag: Token.Tag,

    fn make(comptime str: []const u8, tag: Token.Tag) Self {
        return Self{
            .str = &comptime Codepoint.ctString(str),
            .tag = tag,
        };
    }

    fn eql(self: Self, str: []const Codepoint) bool {
        return if (self.str.len != str.len) x: {
            break :x false;
        } else for (self.str, str) |a, b| {
            if (!a.eql(b)) break false;
        } else true;
    }

    const list = syms: {
        var arr = [_]Self{
            make("&", .ampersand),
            make("{", .lparen),
            make("}", .rparen),
            make("(", .lparen),
            make(")", .rparen),
            make("=", .equals),
            make(":", .colon),
            make(";", .semicolon),
            make(".", .dot),
            make(",", .comma),
            make("-", .minus),
            make("+", .plus),
            make("*", .star),
            make("/", .slash),
            make("%", .percent),
        };

        const by_reverse_len = struct {
            fn lessThan(_: void, a: Self, b: Self) bool {
                return a.str.len > b.str.len;
            }
        }.lessThan;

        std.sort.block(Self, &arr, {}, by_reverse_len);

        break :syms arr;
    };
};

fn skipSpaces(self: *Lexer) Error!void {
    while (try self.peekC()) |pk| {
        if (!pk.isSpace()) return;
        self.acceptC(pk);
    }
}

/// iterate to find the next token
fn lex(self: *Lexer) Error!?Token {
    try self.skipSpaces();

    const start_loc = self.loc;
    const start_index = self.index();
    const start_ch = try self.peekC() orelse return null;

    const tag: Token.Tag = for (Symbol.list) |sym| {
        // symbols
        var buf: [8]Codepoint = undefined;
        const got = try self.peekSlice(&buf, sym.str.len);

        if (sym.eql(got)) {
            self.acceptSlice(got);
            break sym.tag;
        }
    } else if (isIdentStart(start_ch)) tok: {
        // identifiers
        self.acceptC(start_ch);
        while (try self.peekC()) |inner_ch| {
            if (!isIdentTail(inner_ch)) break;
            self.acceptC(inner_ch);
        }

        const text = self.iter.text[start_index..self.index()];
        for (Keyword.list) |kw| {
            if (std.mem.eql(u8, kw.str, text)) {
                break :tok kw.tag;
            }
        }

        break :tok .ident;
    } else if (isDecimal(start_ch)) tok: {
        self.acceptC(start_ch);

        // integral
        while (try self.peekC()) |inner_ch| {
            if (!isDecimal(inner_ch)) break;
            self.acceptC(inner_ch);
        }

        const dot_ch = try self.peekC() orelse {
            break :tok .int;
        };
        if (dot_ch.c != '.') break :tok .int;
        self.acceptC(dot_ch);

        // fractional
        while (try self.peekC()) |inner_ch| {
            if (!isDecimal(inner_ch)) break;
            self.acceptC(inner_ch);
        }

        break :tok .real;
    } else {
        std.debug.print("invalid input: `{}`\n", .{start_ch});
        return Error.InvalidInput;
    };

    const stop_index = self.index();

    const token = Token{
        .loc = start_loc,
        .tag = tag,
        .start = start_index,
        .stop = stop_index,
    };

    if (options.log_lexer) {
        const ally = std.heap.page_allocator;

        var mason = blox.Mason.init(ally);
        defer mason.deinit();

        const rendered = fluent.sources.render(&mason, start_loc) catch |e| {
            std.debug.panic("error in lexer logging: {s}\n", .{@errorName(e)});
        };

        logger.debug("{s} `{s}`\n{}", .{
            @tagName(token.tag),
            self.slice(token),
            mason.fmt(rendered, .{ .enable_colors = false }),
        });
    }

    return token;
}

/// fill cache with the next `count` tokens
/// fails if there aren't enough tokens; returns success
fn cacheTokens(self: *Lexer, count: usize) Error!bool {
    std.debug.assert(count <= TokenCache.cache_len);

    return while (self.cache.len < count) {
        const token = try self.lex() orelse {
            break false;
        };

        self.cache.push(token);
    } else true;
}

pub fn next(self: *Lexer) Error!?Token {
    const token = try self.peek() orelse {
        return null;
    };

    self.cache.advance();

    return token;
}

pub fn peek(self: *Lexer) Error!?Token {
    return self.peekIndex(0);
}

pub fn peekIndex(self: *Lexer, n: usize) Error!?Token {
    if (!try self.cacheTokens(n + 1)) {
        return null;
    }

    return self.cache.get(n);
}

pub fn accept(self: *Lexer, token: Token) void {
    std.debug.assert(std.meta.eql(token, self.cache.get(0)));
    self.cache.advance();
}
