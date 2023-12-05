const std = @import("std");
const Allocator = std.mem.Allocator;
const ZigError = std.builtin.Type.Error;

/// a data structure for collecting errors
///
/// provide a metadata type `T` and an error `e`
pub fn ErrorBuf(comptime T: type, comptime e: anyerror) type {
    return struct {
        const Self = @This();

        /// zig error type
        pub const Error = @Type(.{
            .ErrorSet = &.{
                ZigError{ .name = @errorName(e) },
            },
        });

        arena: std.heap.ArenaAllocator,
        meta: ?T = null,

        pub fn init(ally: Allocator) Self {
            return Self{
                .arena = std.heap.ArenaAllocator.init(ally),
            };
        }

        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }

        /// arena allocator for error metadata
        pub fn allocator(self: *Self) Allocator {
            return self.arena.allocator();
        }

        /// return an error and store some metadata
        pub fn err(self: *Self, meta: T) Error {
            self.meta = meta;
            return @errSetCast(e);
        }

        pub fn FilteredError(comptime E: type) type {
            comptime {
                const info = @typeInfo(E);
                if (info != .ErrorSet) {
                    @compileError(@typeName(E) ++ " is not an error set.");
                }

                const unfiltered: []const ZigError = info.ErrorSet orelse &.{};

                // ensure this set is filterable
                for (unfiltered) |zerr| {
                    if (std.mem.eql(u8, @errorName(e), zerr.name)) {
                        break;
                    }
                } else {
                    const msg =
                        @typeName(E) ++
                        " does not contain " ++
                        @errorName(e);
                    @compileError(msg);
                }

                // create filtered error
                var filtered: [unfiltered.len - 1]ZigError = undefined;
                var i: usize = 0;
                for (unfiltered) |zerr| {
                    if (!std.mem.eql(u8, @errorName(e), zerr.name)) {
                        filtered[i] = zerr;
                        i += 1;
                    }
                }

                return @Type(.{ .ErrorSet = &filtered });
            }
        }

        pub fn FilteredType(comptime U: type) type {
            return union(enum) {
                err: T,
                payload: U,
            };
        }

        pub fn FilteredErrorUnion(comptime Eu: type) type {
            comptime {
                const info = @typeInfo(Eu);
                if (info != .ErrorUnion) {
                    @compileError(@typeName(Eu) ++ " is not an error union.");
                }

                const eu = info.ErrorUnion;
                return FilteredError(eu.error_set)!FilteredType(eu.payload);
            }
        }

        /// filters out the zig error for this buffer, returns either a filtered
        /// error or a FilteredType containing a buffered error or a payload
        pub fn filter(
            self: *Self,
            x: anytype,
        ) FilteredErrorUnion(@TypeOf(x)) {
            // this will be verified when calculating return type
            const info = @typeInfo(@TypeOf(x)).ErrorUnion;
            const Err = FilteredError(info.error_set);
            const Ret = FilteredType(info.payload);

            if (x) |payload| {
                return Ret{ .payload = payload };
            } else |ze| if (std.mem.eql(u8, @errorName(e), @errorName(ze))) {
                return Ret{ .err = self.meta.? };
            } else {
                return @as(Err, @errSetCast(ze));
            }
        }
    };
}
