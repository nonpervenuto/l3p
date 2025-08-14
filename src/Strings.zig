const std = @import("std");

pub fn eqlIgnoreCase(needle: []const u8, keyword: []const u8) bool {
    var buffer: [1024]u8 = undefined;
    if (buffer.len >= needle.len) {
        const upNeedle = std.ascii.upperString(&buffer, needle);
        if (std.mem.eql(u8, upNeedle, keyword)) {
            return true;
        }
    }
    return false;
}
