const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

pub const Number = struct {
    const Self = @This();

    str: []const u8,

    pub const ParseError = Allocator.Error || error{
        InvalidRadix,
        InvalidDigit,
        InvalidNumeric,
        Overflow,
    };

    fn parseDigit(comptime T: type, ch: u8, radix: T) ParseError!T {
        const digit_byte: u8 = switch (ch) {
            '0'...'9' => ch - '0',
            'a'...'z' => 10 + ch - 'a',
            'A'...'Z' => 10 + ch - 'A',
            else => {
                return ParseError.InvalidDigit;
            },
        };

        const digit: T = switch (@typeInfo(T)) {
            .Float => @floatFromInt(digit_byte),
            .Int => digit_byte,
            else => unreachable,
        };

        if (digit >= radix) {
            return ParseError.InvalidDigit;
        }

        return digit;
    }

    /// valid numbers:
    /// - may start with a sign '+' or '-'
    /// - after sign may define radix with '0b' '0o' or '0x' to encode binary,
    ///   octal, or hexadecimal bases. otherwise they are assumed to be decimal.
    /// - contain valid digits for their base (capitalization is ignored), and
    ///   underscores are ignored
    /// - may contain a dot '.' to separate fractal component
    /// - may contain an exponent after 'e' or 'E'
    fn parseInto(self: Self, comptime T: type) ParseError!T {
        std.debug.assert(self.str.len > 0);

        const info = @typeInfo(T);
        var trav = self.str;
        var n: T = 0;

        // detect radix
        var radix: T = 10;
        if (trav.len > 2 and trav[0] == '0') {
            radix = switch (trav[1]) {
                'b' => b: {
                    trav = trav[2..];
                    break :b 2;
                },
                'o' => o: {
                    trav = trav[2..];
                    break :o 8;
                },
                'x' => x: {
                    trav = trav[2..];
                    break :x 16;
                },
                else => 10,
            };
        }

        // parse integral
        var base: T = 1;
        var will_overflow = false;
        const integral_len = std.mem.indexOf(u8, trav, ".") orelse trav.len;
        var integral_iter = std.mem.reverseIterator(trav[0..integral_len]);
        while (integral_iter.next()) |ch| {
            if (ch == '_') continue;
            const digit = try parseDigit(T, ch, radix);

            if (will_overflow) {
                return ParseError.Overflow;
            }

            n += digit * base;
            switch (info) {
                .Int => {
                    base = std.math.mul(T, base, radix) catch {
                        will_overflow = true;
                        continue;
                    };
                },
                .Float => {
                    base *= radix;
                },
                else => unreachable,
            }
        }

        trav = trav[integral_len..];

        // parse fractal
        if (trav.len > 0) {
            switch (info) {
                .Float => {
                    std.debug.assert(info == .Float);
                    std.debug.assert(trav[0] == '.');

                    trav = trav[1..];

                    var fract: T = 0;
                    var fract_base: T = -1;
                    for (trav) |ch| {
                        if (ch == '_') continue;
                        const digit = try parseDigit(T, ch, radix);

                        fract += digit * std.math.pow(T, radix, fract_base);
                        fract_base -= 1;
                    }

                    n += fract;
                },
                else => unreachable,
            }
        }

        return n;
    }
};

test "parse Number into u64" {
    const Int = u64;
    const ints = [_]Int{
        0,
        420,
        std.math.maxInt(Int),
    };
    const fmts = [_][]const u8{
        "{d}",
        "0b{b}",
        "0o{o}",
        "0x{x}",
    };

    for (ints) |n| {
        inline for (fmts) |fmt| {
            var buf: [128]u8 = undefined;
            const str = try std.fmt.bufPrint(&buf, fmt, .{n});
            const num = Number{ .str = str };

            const out = try num.parseInto(Int);
            try std.testing.expectEqual(n, out);
        }
    }
}

test "parse Number into f64" {
    const Float = f64;
    const ints = [_]Float{
        0,
        420,
        std.math.floatMax(Float),
        std.math.floatMin(Float),
    };
    const fmts = [_][]const u8{
        "{d}",
    };

    for (ints) |n| {
        inline for (fmts) |fmt| {
            var buf: [1024]u8 = undefined;
            const str = try std.fmt.bufPrint(&buf, fmt, .{n});
            const num = Number{ .str = str };

            const out = num.parseInto(Float) catch |e| {
                std.debug.print("fail: {} \"{s}\"\n", .{ n, str });
                return e;
            };
            // NOTE this should be perfect, but it's good enough for now
            try std.testing.expectApproxEqRel(n, out, 1e-15);
        }
    }
}
