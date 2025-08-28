const std = @import("std");
const Ir = @import("Ir.zig");
const Compiler = @import("Compiler.zig");
const CodegenWasm = @import("CodegenWasm.zig");
const ArgParser = @import("ArgParser.zig");
const Codegen = @import("Codegen.zig").Codegen;

pub fn main() !void {
    const allocator: std.mem.Allocator = std.heap.page_allocator;

    var argIterator: std.process.ArgIterator = try std.process.argsWithAllocator(allocator);
    defer argIterator.deinit();

    const options = ArgParser.parse(&argIterator);
    std.log.debug("{any}", .{options});

    if (options.path) |path| {
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            std.log.err("Failed to open file: {s}", .{@errorName(err)});
            return;
        };
        defer file.close();
        const buffer = try file.deprecatedReader().readAllAlloc(allocator, 1024 * 1024);
        defer allocator.free(buffer);

        var compiler = Compiler.init(allocator);
        var ir = compiler.compile(path, buffer) catch |err| switch (err) {
            else => return err,
        };

        var builder = Codegen.from(allocator, @tagName(options.target)).?;

        _ = try builder.build(path, &ir);
    }
}
