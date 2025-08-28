const std = @import("std");

const ArgIterator = std.process.ArgIterator;

pub const Target = enum {
    @"linux-x64",
    wasm,
};

pub const Options = struct {
    path: ?[]const u8,
    target: Target,
    run: bool,
};

pub fn parse(args: *ArgIterator) Options {
    var opts: Options = .{
        .path = null,
        .target = .@"linux-x64",
        .run = false,
    };
    // skip program name
    _ = args.skip();
    while (args.next()) |a| {
        // strip 0-sentinel
        const arg = a[0..a.len];
        if (std.mem.startsWith(u8, arg, "--")) {
            // if arg starts with `--`, we can have either:
            // --arg: a boolean switch
            // --arg=value: key/value arg
            var arg_name = arg[2..];
            var arg_value: ?[]const u8 = null;
            const val_index = std.mem.indexOf(u8, arg_name, "=");
            if (val_index) |vi| {
                arg_value = arg_name[vi + 1 ..];
                arg_name = arg_name[0..vi];
            }

            const opts_type_info = @typeInfo(Options);
            const opts_fields_info = opts_type_info.@"struct".fields;
            inline for (opts_fields_info) |fi| {
                if (std.mem.eql(u8, fi.name, arg_name)) {
                    if (getArgValue(fi.type, arg_value)) |fv| {
                        @field(opts, fi.name) = fv;
                    } else {
                        std.log.debug("Cannot find value {s} for {s}\n", .{ arg_value.?, arg_name });
                        printHelp();
                    }
                }
            }
        } else {
            // if arg doesn not start with `--`, assume `path`, multiple occurrences will override
            opts.path = arg;
        }
    }

    if (opts.path == null) {
        printHelp();
    }

    return opts;
}

fn printHelp() void {
    var stdout_writer = std.fs.File.stdout().writer(&.{});
    const stdout = &stdout_writer.interface;
    stdout.print(
        \\
        \\ Usage: l3p <path> [options]
        \\
        \\ Options:
        \\     --target=<target>            Compilation target: linux-x64, wasm. Default: linux-x64
        \\     --run                        Run after compilation. Default: false
        \\
        \\
    , .{}) catch |err| {
        std.debug.print("printHelp() error: {any}\n", .{err});
    };
    stdout.flush() catch |err| {
        std.debug.print("printHelp() flush error: {any}\n", .{err});
    };
}

fn getArgValue(comptime T: type, val: ?[]const u8) ?T {
    std.log.debug("Get arg value: {any}@{any}", .{ T, val });
    const ti = @typeInfo(T);
    switch (ti) {
        .@"enum" => {
            inline for (ti.@"enum".fields) |fe| {
                if (std.mem.eql(u8, fe.name, val.?)) {
                    return @field(T, fe.name);
                }
            }
            return null;
        },
        .array => return val.?,
        .bool => return true,
        else => return null,
    }
}
