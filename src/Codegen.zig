const std = @import("std");
const Ir = @import("Ir.zig");

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) @This() {
    return @This(){
        .allocator = allocator,
    };
}

pub fn write(w: anytype, comptime code: []const u8, args: anytype) !void {
    try w.print(code ++ "\n", args);
}

fn loadReg(w: anytype, reg: []const u8, arg: Ir.Arg) !void {
    switch (arg) {
        .variable => |address| try w.print("  mov {s}, QWORD [rbp - {d}] \n", .{ reg, address }),
        .integerLiteral => |value| try w.print("  mov {s}, QWORD {d} \n", .{ reg, value }),
        .dataLiteral => |value| try w.print("  mov {s}, data{d}+0 \n", .{ reg, value }),
        .deref => |address| {
            try w.print(" mov {s}, [rbp - {d}]\n", .{ reg, address });
            try w.print(" mov {s}, [{s}]\n", .{ reg, reg });
        },
    }
}

pub fn build(self: @This(), ir: *Ir) !void {
    const file = try std.fs.cwd().createFile("output.asm", .{ .truncate = true });
    errdefer file.close();
    var buf = std.io.bufferedWriter(file.deprecatedWriter());
    var w = buf.writer();

    {
        try write(w,
            \\ format ELF64
            \\ section ".text" executable align 8
            \\ public main 
            \\ main: 
            \\   push rbp
            \\   mov rbp, rsp
            \\   extrn putchar
            \\   extrn printf
        , .{});

        const stackSize = ir.getStackSize();

        // allocate stack
        try write(w,
            \\  sub rsp, {d}
        , .{stackSize});

        // TODO
        // RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, sono 64bit
        // EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, sono 32bit

        // assign value
        for (ir.operations.items) |operation| {
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
                .assign => |assign| {
                    const target = assign.offset;
                    try loadReg(w, "  rax", assign.arg);
                    try w.print("  mov QWORD [rbp - {d}], rax \n", .{target});
                },
                .index => |_| {},
                .call => |call| {
                    // try w.print("  ;function call\n", .{});
                    const fnName = call.name;
                    const registers = [_][]const u8{ "RDI", "RSI", "RDX", "RCX", "R8", "R9" };
                    if (call.args.len <= registers.len) {
                        for (call.args, 0..) |arg, i| {
                            const reg = registers[i];
                            switch (arg) {
                                .variable => |offset| try w.print("  mov {s}, [rbp - {d}] \n", .{ reg, offset }),
                                .integerLiteral => |value| try w.print("  mov {s}, {d} \n", .{ reg, value }),
                                .dataLiteral => |value| try w.print("  mov {s}, data{d}+0 \n", .{ reg, value }),
                                .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                            }
                        }
                        // TODO azzera il registo AL, serve per chiamara printf, non so se serve per tutte le funzioni extrn
                        try w.print("  xor eax, eax\n", .{});
                        try w.print("  call {s}\n", .{fnName});
                    } else {
                        @panic("Call a funzioni esterne supporta al massimo 6 argomenti");
                    }
                },
                .prefix_neg => |prefix| {
                    try loadReg(w, "  rax", prefix.arg);
                    try w.print("  neg rax\n", .{});
                    try w.print("  mov [rbp - {d}], rax\n", .{prefix.offset});
                },
                .infix_or => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  or rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  or rax, {d} \n", .{rhs_value}),
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
                        .integerLiteral => |rhs_value| try w.print("  and rax, {d} \n", .{rhs_value}),
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
                        .integerLiteral => |rhs_value| try w.print("  or rax, {d} \n", .{rhs_value}),
                        .dataLiteral => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                        .deref => |_| @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_xor => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  xor rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  xor rax, {d} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_bit_and => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  and rax, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  and rax, {d} \n", .{rhs_value}),
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
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  shl rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  shl rax, {d} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_shift_right => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  shr rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  shr rax, {d} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_plus => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    try loadReg(w, "  rax", infix_plus.lhs);
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try w.print("  add rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  add rax, {d} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_minus => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  sub rax, QWORD [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  sub QWORD rax, {d} \n", .{rhs_value}),
                        else => @panic("Not implemented"),
                    }
                    try w.print("  mov QWORD [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_multiply => |infix| {
                    const target_offset = infix.offset;
                    try loadReg(w, "  rax", infix.lhs);
                    switch (infix.rhs) {
                        .variable => |rhs_offset| try w.print("  imul rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try w.print("  imul rax, {d} \n", .{rhs_value}),
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
                        .integerLiteral => |rhs_value| try w.print("  mov rbx, {d} \n", .{rhs_value}),
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
                        .integerLiteral => |rhs_value| try w.print("  mov rbx, {d} \n", .{rhs_value}),
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
        for (ir.variables.items) |variable| {
            switch (variable) {
                .var_dec => {},
                .global_dec => |global| {
                    if (global.data != null) {
                        try w.print("   data{d}: db ", .{global.address});
                        for (global.data.?, 0..) |char, i| {
                            try w.print("0x{X}", .{char});
                            if (i < global.data.?.len - 1) {
                                try w.print(", ", .{});
                            } else {
                                try w.print(", ", .{});
                            }
                        }
                        try w.print("0x00", .{});
                        try w.print("\n", .{});
                    }
                },
            }
        }
    }
    try buf.flush();
    file.close();

    {
        var cmd = std.process.Child.init(&[_][]const u8{ "fasm", "output.asm" }, self.allocator);
        try cmd.spawn();
        _ = try cmd.wait();
    }

    {
        var cmd = std.process.Child.init(&[_][]const u8{ "gcc", "-no-pie", "-o", "output", "output.o" }, self.allocator);
        try cmd.spawn();
        _ = try cmd.wait();
    }
}
