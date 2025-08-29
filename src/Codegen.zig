const std = @import("std");

const Options = @import("ArgParser.zig").Options;
const CodegenLinux = @import("CodegenLinux.zig");
const CodegenWasm = @import("CodegenWasm.zig");
const Ir = @import("Ir.zig");

pub const Codegen = union(enum) {
    @"linux-x64": CodegenLinux,
    wasm: CodegenWasm,

    pub fn init(self: @This(), allocator: std.mem.Allocator) @This() {
        switch (self) {
            inline else => |case| return case.init(allocator),
        }
    }

    pub fn build(self: @This(), path: []const u8, ir: *Ir) ![]const u8 {
        switch (self) {
            inline else => |case| return case.build(path, ir),
        }
    }

    pub fn from(allocator: std.mem.Allocator, options: Options) ?Codegen {
        const ti = @typeInfo(@This());
        inline for (ti.@"union".fields) |fe| {
            if (std.mem.eql(u8, fe.name, @tagName(options.target))) {
                if (@hasDecl(fe.type, "init")) {
                    const initFn = @field(fe.type, "init");
                    return @unionInit(Codegen, fe.name, initFn(allocator));
                }
            }
        }
        return null;
    }
};
