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
    _ = text;

    @panic("TODO");

    // remember to use muladd
}
