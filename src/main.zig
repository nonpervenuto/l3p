const std = @import("std");
const Ir = @import("Ir.zig");
const Compiler = @import("Compiler.zig");
const CodegenWasm = @import("CodegenWasm.zig");
const ArgParser = @import("ArgParser.zig");
const Codegen = @import("Codegen.zig").Codegen;
const System = @import("System.zig");

pub fn main() !void {
    const gpa: std.mem.Allocator = std.heap.page_allocator;

    // TODO: Error handling argument parsing
    const options = ArgParser.parse(gpa) catch {
        return;
    };

    try compileSource(gpa, options);
}

fn compileSource(gpa: std.mem.Allocator, options: ArgParser.Options) !void {
    if (options.path) |path| {
        const file = std.fs.cwd().openFile(path, .{}) catch |err| {
            System.Err.printRed("Failed to open file: \"{s}\" {s} \n", .{ path, @errorName(err) });
            return;
        };
        defer file.close();
        const buffer = try file.deprecatedReader().readAllAlloc(gpa, 1024 * 1024);
        defer gpa.free(buffer);

        var compiler = Compiler.init(gpa);
        var ir = compiler.compile(path, buffer) catch {
            return;
        };

        var builder = Codegen.from(gpa, options) orelse {
            System.Err.printRed("Invalid target: {s}\n", .{@tagName(options.target)});
            return;
        };

        const executable = try builder.build(path, &ir);
        if (options.run) {
            _ = try runExecutable(gpa, &[_][]const u8{executable});
        }
    }
}

fn runExecutable(gpa: std.mem.Allocator, argv: []const []const u8) !bool {
    var cmd = std.process.Child.init(argv, gpa);
    try cmd.spawn();
    const term = try cmd.wait();
    // Check status code
    return switch (term) {
        .Exited => |code| code == 0,
        else => false,
    };
}
