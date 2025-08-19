///
/// Generic iterators, not used at the moment.
///
const std = @import("std");

pub fn Init(comptime Context: type, comptime T: type, E: type) type {
    return struct {
        pub fn forEach(context: Context, args: anytype, callback: fn (Context, T) E!void) E!void {
            inline for (@TypeOf(args)) |field| {
                try callback(context, @field(args, field.name));
            }
        }

        pub fn all(context: Context, args: anytype, callback: fn (Context, T) bool) bool {
            const fields = std.meta.fields(@TypeOf(args));
            inline for (fields) |field| {
                if (!callback(context, @field(args, field.name))) {
                    return false;
                }
            }
            return true;
        }

        pub fn some(context: Context, args: anytype, callback: fn (Context, T) bool) bool {
            const fields = std.meta.fields(@TypeOf(args));
            inline for (fields) |field| {
                if (callback(context, @field(args, field.name))) {
                    return true;
                }
            }
            return false;
        }
    };
}
