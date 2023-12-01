const std = @import("std");
const Allocator = std.mem.Allocator;
const fluent = @import("../mod.zig");
const env = fluent.env;
const Name = fluent.Name;
const Ident = fluent.Ident;

pub fn ScopedMapUnmanaged(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Managed = ScopedMap(T);
        pub const Entry = struct {
            name: Name,
            value: T,
        };

        scope: Name,
        entries: std.MultiArrayList(Entry) = .{},
        markers: std.ArrayListUnmanaged(usize) = .{},

        pub fn init(scope: Name) Self {
            return .{ .scope = scope };
        }

        pub fn deinit(self: *Self, ally: Allocator) void {
            self.entries.deinit(ally);
            self.markers.deinit(ally);
        }

        /// store a marker to denote the current scope
        pub fn enterScope(
            self: *Self,
            ally: Allocator,
            ident: Ident,
        ) Allocator.Error!void {
            self.scope = try env.namePush(self.scope, ident);
            try self.markers.append(ally, self.entries.len);
        }

        /// remove all of the entries since the last scope
        pub fn exitScope(self: *Self, ally: Allocator) void {
            self.scope = env.nameDrop(self.scope).?;
            const prev_len = self.markers.pop();
            self.markers.shrinkAndFree(ally, prev_len);
        }

        /// add an absolute name to the map
        pub fn putAbsolute(
            self: *Self,
            ally: Allocator,
            abs_name: Name,
            value: T,
        ) Allocator.Error!void {
            try self.entries.append(ally, Entry{
                .name = abs_name,
                .value = value,
            });
        }

        /// add a relative name to the map
        pub fn put(
            self: *Self,
            ally: Allocator,
            rel_name: Name,
            value: T,
        ) Allocator.Error!void {
            const abs_name = try env.nameCat(self.scope, rel_name);
            try self.putAbsolute(ally, abs_name, value);
        }

        /// attempt to retrieve an absolute name from the map
        pub fn getAbsolute(self: Self, abs_name: Name) ?T {
            var names = std.mem.reverseIterator(self.entries.items(.name));
            var i: usize = self.entries.len - 1;
            while (names.next()) |name| : (i -= 1) {
                if (name.eql(abs_name)) {
                    return self.entries.items(.value)[i];
                }
            }

            return null;
        }

        /// attempt to retrieve an relative name from the map
        pub fn get(self: Self, rel_name: Name) Allocator.Error!?T {
            const abs_name = try env.nameCat(self.scope, rel_name);
            return self.getAbsolute(abs_name);
        }
    };
}

pub fn ScopedMap(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const Unmanaged = ScopedMapUnmanaged(T);
        pub const Entry = Unmanaged.Entry;

        ally: Allocator,
        unmanaged: Unmanaged,

        pub fn init(ally: Allocator, scope: Name) Self {
            return Self{
                .ally = ally,
                .unmanaged = Unmanaged.init(scope),
            };
        }

        pub fn deinit(self: *Self) void {
            self.unmanaged.deinit(self.ally);
        }

        pub fn enterScope(self: *Self, ident: Ident) Allocator.Error!void {
            try self.unmanaged.enterScope(self.ally, ident);
        }

        pub fn exitScope(self: *Self) void {
            self.unmanaged.exitScope(self.ally);
        }

        pub fn putAbsolute(
            self: Self,
            abs_name: Name,
            value: T,
        ) Allocator.Error!void {
            try self.unmanaged.putAbsolute(self.ally, abs_name, value);
        }

        pub fn put(self: *Self, rel_name: Name, value: T) Allocator.Error!void {
            try self.unmanaged.put(self.ally, rel_name, value);
        }

        // TODO putIdent and getIdent

        pub fn getAbsolute(self: Self, abs_name: Name) ?T {
            return self.unmanaged.getAbsolute(abs_name);
        }

        pub fn get(self: Self, rel_name: Name) Allocator.Error!?T {
            return self.unmanaged.get(rel_name);
        }
    };
}
