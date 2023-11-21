const std = @import("std");
const builtin = @import("builtin");
const fluent = @import("../mod.zig");
const Type = fluent.Type;

/// recursively hashes Type with awareness of self referencing
fn autoHashType(
    comptime T: type,
    x: T,
    this: Type.Id,
    hasher: *TypeSetContext.Hasher,
) void {
    const b = std.mem.asBytes;

    switch (T) {
        Type => {
            hasher.update(b(&@as(Type.Tag, x)));

            switch (x) {
                inline else => |data| {
                    autoHashType(@TypeOf(data), data, this, hasher);
                },
            }
        },

        // hash id with awareness of self
        Type.Id => {
            if (this.eql(x)) {
                // the constant used here does not matter (in terms of
                // correctness) as long as it's a constant
                hasher.update("this");
            } else {
                hasher.update(b(&x));
            }
        },

        // hash value directly
        void,
        fluent.Ident,
        fluent.Name,
        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => {
            hasher.update(b(&x));
        },

        // hash slice of hashable
        []const Type.Id,
        []const Type.Class.Member,
        => {
            for (x) |elem| {
                autoHashType(@TypeOf(elem), elem, this, hasher);
            }
        },

        // hash struct fields
        Type.Int,
        Type.Float,
        Type.Fn,
        Type.Class,
        Type.Class.Member,
        Type.Struct,
        => {
            inline for (@typeInfo(T).Struct.fields) |field| {
                autoHashType(field.type, @field(x, field.name), this, hasher);
            }
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("cannot hash type {}", .{T});
            } else unreachable;
        },
    }
}

/// recursively checks Type for equality with awareness of self referencing
fn autoEqlType(
    comptime T: type,
    a: T,
    this_a: Type.Id,
    b: T,
    this_b: Type.Id,
) bool {
    return switch (T) {
        // trivial equalities
        void => true,

        Type.Int.Signedness,
        Type.Int.Bits,
        Type.Float.Bits,
        => a == b,

        fluent.Ident, fluent.Name => a.eql(b),

        // slice of autoEql-able
        []const Type.Id,
        []const Type.Class.Member,
        => eql: {
            if (a.len != b.len) {
                break :eql false;
            }

            for (a, b) |elem_a, elem_b| {
                const elems_eql = autoEqlType(
                    @TypeOf(elem_a, elem_b),
                    elem_a,
                    this_a,
                    elem_b,
                    this_b,
                );

                if (!elems_eql) break :eql false;
            }

            break :eql true;
        },

        // structs of hashable
        Type.Fn,
        Type.Class,
        Type.Class.Member,
        Type.Struct,
        => eql: {
            inline for (@typeInfo(T).Struct.fields) |field| {
                const fields_eql = autoEqlType(
                    field.type,
                    @field(a, field.name),
                    this_a,
                    @field(b, field.name),
                    this_b,
                );

                if (!fields_eql) break :eql false;
            }

            break :eql true;
        },

        Type.Id => eql: {
            const a_is_this = a.eql(this_a);
            const b_is_this = b.eql(this_b);

            // either they are both this, or neither are this and they are
            // the same id
            break :eql a_is_this and b_is_this or
                !a_is_this and !b_is_this and a.eql(b);
        },

        Type => eql: {
            if (@as(Type.Tag, a) != @as(Type.Tag, b)) {
                break :eql false;
            }

            switch (a) {
                inline else => |data_a, tag| {
                    const data_b = @field(b, @tagName(tag));
                    break :eql autoEqlType(
                        @TypeOf(data_a, data_b),
                        data_a,
                        this_a,
                        data_b,
                        this_b,
                    );
                },
            }
        },

        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("cannot auto eql two of {}", .{T});
            } else unreachable;
        },
    };
}

const TypeSetContext = struct {
    const Hasher = std.hash.Wyhash;
    const seed = 0xBEEF_FA75;

    pub fn hash(_: @This(), entry: TypeSetEntry) u64 {
        var hasher = Hasher.init(seed);
        autoHashType(Type, entry.type.*, entry.id, &hasher);
        return hasher.final();
    }

    pub fn eql(_: @This(), a: TypeSetEntry, b: TypeSetEntry) bool {
        return autoEqlType(Type, a.type.*, a.id, b.type.*, b.id);
    }
};

pub const TypeSetEntry = struct {
    id: Type.Id,
    type: *const Type,
};

pub const TypeSet = std.HashMapUnmanaged(
    TypeSetEntry,
    void,
    TypeSetContext,
    std.hash_map.default_max_load_percentage,
);
