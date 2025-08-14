const std = @import("std");

pub fn eqlIgnoreCase(needle: []const u8, haystack: []const u8) bool {
    return std.ascii.eqlIgnoreCase(needle, haystack);
    // var buffer1: [1024]u8 = undefined;
    // var buffer2: [1024]u8 = undefined;
    // if (buffer1.len >= needle.len and buffer2.len >= haystack.len) {
    //     const upNeedle = std.ascii.upperString(&buffer1, needle);
    //     const upHaystack = std.ascii.upperString(&buffer2, haystack);
    //     if (std.mem.eql(u8, upNeedle, upHaystack)) {
    //         return true;
    //     }
    // }
    // return false;
}
