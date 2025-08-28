const std = @import("std");
const Ir = @import("Ir.zig");

allocator: std.mem.Allocator,
std_err_writer: std.fs.File.Writer,

pub const CodegenError = error{
    FasmError,
    GccError,
    OutOfMemory,
};

pub fn init(allocator: std.mem.Allocator) @This() {
    const buf = allocator.alloc(u8, 4096) catch unreachable;
    return @This(){
        .allocator = allocator,
        .std_err_writer = std.fs.File.stderr().writer(buf),
    };
}

pub fn write(w: anytype, comptime code: []const u8, args: anytype) !void {
    try w.print(code ++ "\n", args);
}

fn getFileName(self: @This(), path: []const u8, ext: ?[]const u8) ![]const u8 {
    var file_name = std.fs.path.basename(path);
    const index = std.mem.lastIndexOfScalar(u8, file_name, '.') orelse 0;
    file_name = if (index == 0) file_name else file_name[0..index];

    if (ext) |e| {
        return try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ file_name, e });
    } else {
        return try std.fmt.allocPrint(self.allocator, "{s}", .{file_name});
    }
}

pub fn build(self: @This(), path: []const u8, ir: *Ir) ![]const u8 {
    const cwd = std.fs.cwd();
    cwd.makeDir("l3p-out-wasm") catch {};

    const file_name_asm = try self.getFileName(path, "wat");
    const file_path_asm = try std.fmt.allocPrint(self.allocator, "l3p-out-wasm/{s}", .{file_name_asm});
    const file = try std.fs.cwd().createFile(file_path_asm, .{ .truncate = true });
    errdefer file.close();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = file.writer(&stdout_buffer);
    const w = &stdout_writer.interface;

    {
        try write(w,
            \\ (module
            \\   (func $core_log (import "core" "log") (param i32 i32))
            \\   (func $core_timestamp (import "core" "timestamp") (result i64))
            \\   (func $core_panic (import "core" "panic") (param i32 i32))
            \\   (func $std_printf (import "std" "printf") (param i32 i32))
            \\
            \\   (memory $memory0 1)
            \\   (export "memory" (memory $memory0))
            \\   (export "main" (func $main))
            \\
        , .{});

        var off: usize = 0;
        var map = std.AutoHashMap(usize, struct { usize, usize }).init(self.allocator);
        // data section
        for (ir.variables.items) |variable| {
            switch (variable) {
                .var_dec => {},
                .global_dec => |global| {
                    if (global.data != null) {
                        try w.print("   (data (i32.const {d}) \"", .{off});
                        try std.zig.stringEscape(global.data.?, w);
                        try w.print("\")\n", .{});
                        try map.put(global.address, .{ off, global.data.?.len });
                        off = off + global.data.?.len;
                    }
                },
            }
        }

        try write(w,
            \\   (func $main
        , .{});
        // const stackSize = ir.getStackSize();
        // // allocate stack
        // try write(w,
        //     \\  sub rsp, {d}
        // , .{stackSize});

        // TODO
        // RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, sono 64bit
        // EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, sono 32bit

        // assign value
        for (ir.operations.items) |operation| {
            switch (operation) {
                .label => |label| {
                    _ = label;
                    // try w.print("{s}: \n", .{label});
                },
                .jump => |label| {
                    _ = label;
                    // try w.print("  jmp {s}\n", .{label});
                },
                .jump_if_false => |jump_if_false| {
                    _ = jump_if_false;
                    // const label = jump_if_false.label;
                    // const arg = jump_if_false.arg;
                    // try loadReg(w, "  rax", arg);
                    // try w.print("  cmp rax, 0\n", .{});
                    // try w.print("  je {s}\n", .{label});
                },
                // Associativity Right to Left
                .assign => |assign| {
                    _ = assign;
                    // try loadReg(w, "  rax", assign.rhs);
                    // switch (assign.lhs) {
                    //     .variable => |offset| try w.print("  mov [rbp - {d}], rax \n", .{offset}),
                    //     .deref => |offset| {
                    //         try w.print("  mov RBX,  [rbp - {d}] \n", .{offset});
                    //         try w.print("  MOV [RBX], RAX  \n", .{});
                    //     },
                    //     else => @panic("Not implemented"),
                    // }
                },
                .ref => |ref| {
                    _ = ref;
                    // switch (ref.arg) {
                    //     .variable => |address| try w.print("  lea RAX, [rbp - {d}]\n", .{address}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax \n", .{ref.offset});
                },
                .store => |_| {
                    @panic("Not implemented");
                    // try w.print("  mov RAX, [rbp - {d}] \n", .{store.offset});
                    // try loadReg(w, "  RBX", store.arg);
                    // try w.print("  MOV [RAX], RBX  \n", .{});
                },
                .index => |index| {
                    _ = index;
                    // // address di var
                    // try w.print("  lea RAX, [rbp - {d}]\n", .{index.var_address});
                    //
                    // // carico l'indice e l'ho moltiplico per 8
                    // try loadReg(w, "  RBX", index.var_index);
                    // try w.print("  IMUL RBX, 8\n", .{});
                    //
                    // // modifiy address
                    // try w.print("  ADD RAX, RBX\n", .{});
                    // try w.print("  mov [rbp - {d}], rax \n", .{index.offset});
                },
                .call => |call| {
                    const fnName = call.name;
                    for (call.args) |arg| {
                        switch (arg) {
                            .dataLiteral => |dl| {
                                const data_s = map.get(dl);
                                try w.print("    i32.const {d}\n", .{data_s.?.@"0"});
                                try w.print("    i32.const {d}\n", .{data_s.?.@"1"});
                            },
                            else => @panic("Not impletmented"),
                        }
                    }
                    try w.print("    call $std_{s}\n", .{fnName});
                },
                .unary_neg => |prefix| {
                    _ = prefix;
                    // try loadReg(w, "  rax", prefix.arg);
                    // try w.print("  neg rax\n", .{});
                    // try w.print("  mov [rbp - {d}], rax\n", .{prefix.offset});
                },
                .unary_not => |prefix| {
                    _ = prefix;
                    // try loadReg(w, "  rax", prefix.arg);
                    // try w.print("  not rax\n", .{});
                    // try w.print("  mov [rbp - {d}], rax\n", .{prefix.offset});
                },
                .infix_or => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  or rax, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  or rax, {d} \n", .{rhs_value}),
                    //     .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    //     .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_and => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  and rax, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  and rax, {d} \n", .{rhs_value}),
                    //     .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    //     .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_or => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  or rax, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  or rax, {d} \n", .{rhs_value}),
                    //     .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    //     .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_xor => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  xor rax, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  xor rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_and => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  and rax, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  and rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_eq => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  sete dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_not_eq => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  setne dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_l => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  setl dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_le => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  setle dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_ge => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  setge dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_g => |infix| {
                    _ = infix;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // try loadReg(w, "  rbx", infix.rhs);
                    // try w.print("  cmp rax, rbx\n", .{});
                    // try w.print("  setg dl\n", .{});
                    // try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_shift_left => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  shl rax, [rbp - {d}] \n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  shl rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_shift_right => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  shr rax, [rbp - {d}] \n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  shr rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_plus => |infix_plus| {
                    _ = infix_plus;
                    // const target_offset = infix_plus.offset;
                    // try loadReg(w, "  rax", infix_plus.lhs);
                    // switch (infix_plus.rhs) {
                    //     .variable => |rhs_offset| try w.print("  add rax, [rbp - {d}] \n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  add rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_minus => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  sub rax, QWORD [rbp - {d}] \n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  sub QWORD rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov QWORD [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_multiply => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  imul rax, [rbp - {d}] \n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  imul rax, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_divide => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try w.print("  xor rdx, rdx\n", .{});
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  mov rbx, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  div rbx\n", .{});
                    // try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_modulo => |infix| {
                    _ = infix;
                    // const target_offset = infix.offset;
                    // try w.print("  xor rdx, rdx\n", .{});
                    //
                    // try loadReg(w, "  rax", infix.lhs);
                    // switch (infix.rhs) {
                    //     .variable => |rhs_offset| try w.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                    //     .integerLiteral => |rhs_value| try w.print("  mov rbx, {d} \n", .{rhs_value}),
                    //     else => @panic("Not implemented"),
                    // }
                    // try w.print("  div rbx\n", .{});
                    // try w.print("  mov QWORD [rbp - {d}], rdx\n", .{target_offset});
                },
            }
        }

        // deallocate stack
        // try write(w,
        //     \\  add rsp, {d}
        // , .{stackSize});

        // restore stack
        // try write(w,
        //     \\  pop rbp
        //     \\  mov rax, 0
        //     \\  ret
        //     \\
        // , .{});

        try write(w,
            \\      return
            \\   )
            \\ )
        , .{});
    }
    try w.flush();
    file.close();

    const wat_name = try self.getFileName(path, "wat");
    const wat_path = try std.fmt.allocPrint(self.allocator, "l3p-out-wasm/{s}", .{wat_name});
    const wasm_name = try self.getFileName(path, "wasm");
    const wasm_path = try std.fmt.allocPrint(self.allocator, "l3p-out-wasm/{s}", .{wasm_name});
    std.log.info("Creating: {s}", .{wasm_path});
    var cmd = std.process.Child.init(&[_][]const u8{ "wat2wasm", wat_path, "-o", wasm_path }, self.allocator);

    try cmd.spawn();
    const term = cmd.wait() catch |e| {
        std.log.err("Error running wat2wasm: {t}", .{e});
        return error.GccError;
    };

    // Check status code
    if (!switch (term) {
        .Exited => |code| code == 0,
        else => false,
    }) return error.GccError;

    return wasm_name;
}
