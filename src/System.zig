const std = @import("std");

var out = std.fs.File.stdout().writer(&.{});
const std_out = &out.interface;
var stdout_mutex = std.Thread.Mutex.Recursive.init;

var err = std.fs.File.stderr().writer(&.{});
const std_err = &err.interface;
var stderr_mutex = std.Thread.Mutex.Recursive.init;

fn printAndFlush(w: *std.Io.Writer, comptime fmt: []const u8, args: anytype) void {
    w.print(fmt, args) catch {};
    w.flush() catch {};
}

pub const Err = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) void {
        stderr_mutex.lock();
        defer stderr_mutex.unlock();
        printAndFlush(std_out, fmt, args);
    }
    pub fn printRed(comptime fmt: []const u8, args: anytype) void {
        // stderr_mutex.lock();
        // defer stderr_mutex.unlock();
        printAndFlush(std_out, "\x1b[31m" ++ fmt ++ "\x1b[0m", args);
    }
};

pub const Out = struct {
    pub fn print(comptime fmt: []const u8, args: anytype) void {
        stdout_mutex.lock();
        defer stdout_mutex.unlock();
        printAndFlush(std_err, fmt, args);
    }
};
