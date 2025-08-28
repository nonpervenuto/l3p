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

fn loadReg(w: anytype, reg: []const u8, arg: Ir.Arg) !void {
    switch (arg) {
        .variable => |address| try w.print("  mov {s}, [rbp - {d}] \n", .{ reg, address }),
        .integerLiteral => |value| try w.print("  mov {s}, 0x{X} \n", .{ reg, value }),
        .dataLiteral => |value| try w.print("  mov {s}, data{d}+0 \n", .{ reg, value }),
        .deref => |address| {
            try w.print(" mov {s}, [rbp - {d}]\n", .{ reg, address });
            try w.print(" mov {s}, [{s}]\n", .{ reg, reg });
        },
    }
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
    cwd.makeDir("l3p-out") catch {};

    const file_name_asm = try self.getFileName(path, "asm");
    const file_path_asm = try std.fmt.allocPrint(self.allocator, "l3p-out/{s}", .{file_name_asm});
    const file = try std.fs.cwd().createFile(file_path_asm, .{ .truncate = true });

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = file.writer(&stdout_buffer);
    const w = &stdout_writer.interface;

    {
        try write(w,
            \\ format ELF64
            \\ section ".text" executable align 8
            \\ public main 
            \\ main: 
            \\   push rbp
            \\   mov rbp, rsp
            \\   extrn putchar
            \\   extrn getchar
            \\   extrn printf
            \\   extrn scanf
            \\   extrn fopen
            \\   extrn fgetc
            \\   extrn fclose
        , .{});

        const stackSize = ir.getStackSize(&ir.body);

        // allocate stack
        try write(w,
            \\  sub rsp, {d}
        , .{stackSize});

        // TODO
        // RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, sono 64bit
        // EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, sono 32bit

        // assign value
        for (ir.body.operations.items) |operation| {
            switch (operation) {
                .label => |label| {
                    try w.print("{s}: \n", .{label});
                },
                .jump => |label| {
                    try w.print("  jmp {s}\n", .{label});
                },
                .jump_if_false => |jump_if_false| {
                    const label = jump_if_false.label;
                    const arg = jump_if_false.arg;
                    try loadReg(w, "  rax", arg);
                    try w.print("  cmp rax, 0\n", .{});
                    try w.print("  je {s}\n", .{label});
                },
                // Associativity Right to Left
                .assign => |assign| {
                    try loadReg(w, "  rax", assign.rhs);
                    switch (assign.lhs) {
                        .variable => |offset| try w.print("  mov [rbp - {d}], rax \n", .{offset}),
                        .deref => |offset| {
                            try w.print("  mov RBX,  [rbp - {d}] \n", .{offset});
                            try w.print("  MOV [RBX], RAX  \n", .{});
                        },
                        else => @panic("Not implemented"),
                    }
                },
                .ref => |ref| {
                    switch (ref.arg) {
                        .variable => |address| try w.print("  lea RAX, [rbp - {d}]\n", .{address}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax \n", .{ref.offset});
                },
                .store => |_| {
                    @panic("Not implemented");
                    // try w.print("  mov RAX, [rbp - {d}] \n", .{store.offset});
                    // try loadReg(w, "  RBX", store.arg);
                    // try w.print("  MOV [RAX], RBX  \n", .{});
                },
                .index => |index| {
                    // address di var
                    try w.print("  lea RAX, [rbp - {d}]\n", .{index.var_address});

                    // carico l'indice e l'ho moltiplico per 8
                    try loadReg(w, "  RBX", index.var_index);
                    try w.print("  IMUL RBX, 8\n", .{});

                    // modifiy address
                    try w.print("  ADD RAX, RBX\n", .{});
                    try w.print("  mov [rbp - {d}], rax \n", .{index.offset});
                },
                .call => |call| {
                    // try w.print("  ;function call\n", .{});
                    const fnName = call.name;
                    const registers = [_][]const u8{ "RDI", "RSI", "RDX", "RCX", "R8", "R9" };
                    if (call.args.len <= registers.len) {
                        for (call.args, 0..) |arg, i| {
                            const reg = registers[i];
                            try loadReg(w, reg, arg);
                        }
                        // TODO azzera il registo AL, serve per chiamara printf, non so se serve per tutte le funzioni extrn
                        try w.print("  xor eax, eax\n", .{});
                        try w.print("  call {s}\n", .{fnName});
                        // The result of the call is placed in RAX
                        try w.print("  mov [rbp - {d}], rax\n", .{call.offset});
                    } else {
                        @panic("Call a funzioni esterne supporta al massimo 6 argomenti");
                    }
                },
                .unary_neg => |prefix| {
                    try loadReg(w, "  rax", prefix.arg);
                    try w.print("  neg rax\n", .{});
                    try w.print("  mov [rbp - {d}], rax\n", .{prefix.offset});
                },
                .unary_not => |prefix| {
                    try loadReg(w, "  rax", prefix.arg);
                    try w.print("  not rax\n", .{});
                    try w.print("  mov [rbp - {d}], rax\n", .{prefix.offset});
                },
                .infix_or => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  or rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  or rax, 0x{X} \n", .{rhs_value}),
                        .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                        .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_and => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  and rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  and rax, 0x{X} \n", .{rhs_value}),
                        .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                        .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_or => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  or rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  or rax, 0x{X} \n", .{rhs_value}),
                        .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                        .deref => |offset| {
                            try w.print("  mov RBX,  [rbp - {d}] \n", .{offset});
                            try w.print("  OR [RBX], RAX  \n", .{});
                        },
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_xor => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  xor rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  xor rax, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_and => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  and rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  and rax, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_eq => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  sete dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_not_eq => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  setne dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_l => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  setl dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_le => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  setle dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_ge => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  setge dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_g => |infix| {
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    try loadReg(w, "  rbx", infix.rhs);
                    try w.print("  cmp rax, rbx\n", .{});
                    try w.print("  setg dl\n", .{});
                    try w.print("  mov [rbp - {d}], rdx\n", .{infix.offset});
                },
                .infix_shift_left => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  RAX", infix.lhs);
                    try loadReg(w, "  RCX", infix.rhs);
                    try w.print("  SHL RAX, CL \n", .{});
                    try w.print("  mov [rbp - {d}], RAX\n", .{target_offset});
                },
                .infix_shift_right => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  RAX", infix.lhs);
                    try loadReg(w, "  RCX", infix.rhs);
                    try w.print("  SHR RAX, CL \n", .{});
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_plus => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    try loadReg(w, "  rax", infix_plus.lhs);
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try w.print("  add rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  add rax, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_minus => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  sub rax, QWORD [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  sub QWORD rax, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov QWORD [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_multiply => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  imul rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  imul rax, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_divide => |infix| {
                    const target_offset = infix.offset;
                    try w.print("  xor rdx, rdx\n", .{});
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  mov rbx, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  div rbx\n", .{});
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_modulo => |infix| {
                    const target_offset = infix.offset;
                    try w.print("  xor rdx, rdx\n", .{});

                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  mov rbx, 0x{X} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  div rbx\n", .{});
                    try w.print("  mov QWORD [rbp - {d}], rdx\n", .{target_offset});
                },
            }
        }

        // deallocate stack
        try write(w,
            \\  add rsp, {d}
        , .{stackSize});

        // restore stack
        try write(w,
            \\  pop rbp
            \\  mov rax, 0
            \\  ret
            \\ 
        , .{});

        // data section
        try w.print("section \".data\"\n", .{});
        for (ir.globals.items) |global| {
            if (global.data != null) {
                try w.print("   data{d}: db ", .{global.address});
                for (global.data.?) |char| {
                    try w.print("0x{X},", .{char});
                }
                try w.print("0x00\n", .{});
            }
        }
    }
    try w.flush();
    file.close();

    {
        const asm_name = try self.getFileName(path, "asm");
        const asm_path = try std.fmt.allocPrint(self.allocator, "l3p-out/{s}", .{asm_name});
        var cmd = std.process.Child.init(&[_][]const u8{ "fasm", asm_path }, self.allocator);

        try cmd.spawn();
        const term = cmd.wait() catch {
            return error.FasmError;
        };

        // Check status code
        if (!switch (term) {
            .Exited => |code| code == 0,
            else => false,
        }) return error.GccError;
    }

    {
        const exe_name = try self.getFileName(path, null);
        const exe_path = try std.fmt.allocPrint(self.allocator, "l3p-out/{s}", .{exe_name});
        const o_name = try self.getFileName(path, "o");
        const o_path = try std.fmt.allocPrint(self.allocator, "l3p-out/{s}", .{o_name});
        var cmd = std.process.Child.init(&[_][]const u8{ "gcc", "-no-pie", "-o", exe_path, o_path }, self.allocator);
        try cmd.spawn();
        const term = cmd.wait() catch {
            return error.GccError;
        };

        // Check status code
        if (!switch (term) {
            .Exited => |code| code == 0,
            else => false,
        }) return error.GccError;

        return exe_name;
    }
}
