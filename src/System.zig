const std = @import("std");

var out = std.fs.File.stdout().writer(&.{});
const std_out = &out.interface;

var err = std.fs.File.stderr().writer(&.{});
const std_err = &err.interface;

fn printAndFlush(w: *std.Io.Writer, comptime fmt: []const u8, args: anytype) void {
    w.print(fmt, args) catch {};
    w.flush() catch {};
}

pub const Err = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) void {
        printAndFlush(std_out, fmt, args);
    }
    pub fn printRed(comptime fmt: []const u8, args: anytype) void {
        printAndFlush(std_out, "\x1b[31m" ++ fmt ++ "\x1b[0m", args);
    }
};

pub const Out = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) void {
        printAndFlush(std_err, fmt, args);
    }
};
