const std = @import("std");
const com = @import("common");
const Codepoint = com.utf8.Codepoint;

const LexError = error { InvalidInput };
pub const Error =
    Codepoint.ParseError ||
    LexError;

pub const Token = struct {
    pub const Tag = enum {
        ident,
        int,
        real,

        lparen,
        rparen,
        lcurly,
        rcurly,

        colon,
        comma,
        minus,
        plus,
        star,
        slash,
    };

    tag: Tag,
    start: u32,
    stop: u32,
};

const Lexer = @This();

const CodepointCache = com.BoundedRingBuffer(Codepoint, 8);
const TokenCache = com.BoundedRingBuffer(Token, 8);

iter: Codepoint.Iterator,
cache: TokenCache = .{},

pub fn init(text: []const u8) Lexer {
    return .{ .iter = Codepoint.parse(text) };
}

pub fn slice(self: Lexer, tok: Token) []const u8 {
    return self.iter.text[tok.start .. tok.stop];
}

// forwarded codepoint iterator ================================================

fn index(self: *const Lexer) u32 {
    return @intCast(self.iter.byte_index);
}

fn peekC(self: *Lexer) Error!?Codepoint {
    return self.iter.peek();
}

fn acceptC(self: *Lexer, c: Codepoint) void {
    self.iter.accept(c);
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

    fn make(str: []const u8) Self {
        return .{ .str = str };
    }

    const list = [_]Keyword{
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
            make("{", .lparen),
            make("}", .rparen),
            make("(", .lparen),
            make(")", .rparen),
            make(":", .colon),
            make(",", .comma),
            make("-", .minus),
            make("+", .plus),
            make("*", .star),
            make("/", .slash),
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
    while (true) {
        const pk = try self.peekC() orelse return;
        if (!pk.isSpace()) return;
        self.acceptC(pk);
    }
}

/// iterate to find the next token
fn lex(self: *Lexer) Error!?Token {
    try self.skipSpaces();

    const start_index = self.index();
    const start_ch = try self.peekC() orelse return null;

    const tag: Token.Tag = for (Symbol.list) |sym| {
        // symbols
        var buf: [8]Codepoint = undefined;
        const got = try self.iter.peekSlice(&buf, sym.str.len);

        if (sym.eql(got)) {
            self.iter.acceptSlice(got);
            break sym.tag;
        }
    } else if (isIdentStart(start_ch)) tok: {
        // identifiers
        self.acceptC(start_ch);
        while (try self.peekC()) |inner_ch| {
            if (!isIdentTail(inner_ch)) break;
            self.acceptC(inner_ch);
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

    return Token{
        .tag = tag,
        .start = start_index,
        .stop = stop_index,
    };
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