//! TODO I should definitely use std.math.big numbers for these instead!

const std = @import("std");

pub const Int = u64;
pub const ParseDecimalIntError = error{BadDecimalInt};

pub fn parseDecimalInt(text: []const u8) ParseDecimalIntError!Int {
    var n: Int = 0;
    for (text) |ch| {
        const digit: Int = switch (ch) {
            '0'...'9' => ch - '0',
            else => return ParseDecimalIntError.BadDecimalInt,
        };

        n = n * 10 + digit;
    }

    return n;
}

pub const Real = f64;
pub const ParseDecimalRealError = error{BadDecimalReal};

pub fn parseDecimalReal(text: []const u8) ParseDecimalRealError!Real {
    var index: usize = 0;

    // integral part
    var int: Real = 0.0;
    while (text[index] != '.') : (index += 1) {
        const ch = text[index];
        const digit: Real = switch (ch) {
            '0'...'9' => @floatFromInt(ch - '0'),
            else => return ParseDecimalRealError.BadDecimalReal,
        };

        int = @mulAdd(Real, int, 10.0, digit);
    }

    // skip dot
    index += 1;

    // fractional part
    var fract: Real = 0.0;
    var exp: Real = -1.0;
    while (index < text.len) : ({
        index += 1;
        exp -= 1.0;
    }) {
        const ch = text[index];
        const digit: Real = switch (ch) {
            '0'...'9' => @floatFromInt(ch - '0'),
            else => return ParseDecimalRealError.BadDecimalReal,
        };

        const mult = std.math.pow(Real, 10.0, exp);
        fract = @mulAdd(Real, digit, mult, fract);
    }

    return int + fract;
}
