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

const Self = @This();

iter: Codepoint.Iterator,

pub fn init(text: []const u8) Self {
    return .{ .iter = Codepoint.parse(text) };
}

pub fn slice(self: Self, tok: Token) []const u8 {
    return self.iter.text[tok.start .. tok.stop];
}

// forwarded codepoint iterator ================================================

fn index(self: *const Self) u32 {
    return @intCast(self.iter.byte_index);
}

fn next(self: *Self) Error!?Codepoint {
    return self.iter.next();
}

fn peek(self: *Self) Error!?Codepoint {
    return self.iter.peek();
}

fn accept(self: *Self, c: Codepoint) void {
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

const SingleCodepointSymbol = struct {
    const Scs = @This();

    c: Codepoint,
    tag: Token.Tag,

    fn make(comptime str: []const u8, tag: Token.Tag) Scs {
        return .{ .c = Codepoint.ct(str), .tag = tag };
    }

    const list = [_]Scs{
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
};

fn skipSpaces(self: *Self) Error!void {
    while (true) {
        const pk = try self.peek() orelse return;
        if (!pk.isSpace()) return;
        self.accept(pk);
    }
}

pub fn nextToken(self: *Self) Error!?Token {
    try self.skipSpaces();

    const start_index = self.index();
    const start_ch = try self.peek() orelse return null;

    const tag: Token.Tag = if (isIdentStart(start_ch)) tok: {
        // identifiers
        self.accept(start_ch);
        while (try self.peek()) |inner_ch| {
            if (!isIdentTail(inner_ch)) break;
            self.accept(inner_ch);
        }

        break :tok .ident;
    } else if (isDecimal(start_ch)) tok: {
        self.accept(start_ch);

        // integral
        while (try self.peek()) |inner_ch| {
            if (!isDecimal(inner_ch)) break;
            self.accept(inner_ch);
        }

        const dot_ch = try self.peek() orelse {
            break :tok .int;
        };
        if (dot_ch.c != '.') break :tok .int;
        self.accept(dot_ch);

        // fractional
        while (try self.peek()) |inner_ch| {
            if (!isDecimal(inner_ch)) break;
            self.accept(inner_ch);
        }

        break :tok .real;
    } else tok: for (SingleCodepointSymbol.list) |scs| {
        // single codepoint symbols
        if (start_ch.eql(scs.c)) {
            self.accept(start_ch);
            break :tok scs.tag;
        }
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