const std = @import("std");
const Strings = @import("Strings.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

const ArgType = enum { variable, integerLiteral, dataLiteral };
const Arg = union(ArgType) { variable: usize, integerLiteral: i32, dataLiteral: usize };

const DataType = enum { numeric };
const DeclarationType = enum { var_dec, global_dec };
const Declaration = union(DeclarationType) {
    var_dec: struct {
        name: []const u8,
        offset: usize,
        type: DataType,
    },
    global_dec: struct {
        data: []const u8,
        address: usize,
    },
};
const VariableList = std.ArrayList(Declaration);

// expressions
const InstructionType = enum { assign, infix_plus, call };
const Instruction = union(InstructionType) {
    assign: struct {
        offset: usize,
        arg: Arg,
    },
    infix_plus: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    call: struct {
        name: []const u8,
        args: []const Arg,
    },
};
const OperationList = std.ArrayList(Instruction);

allocator: std.mem.Allocator,
variables: VariableList,
operations: OperationList,

fn findVariable(self: *@This(), name: []const u8) ?Declaration {
    for (self.variables.items) |variable| {
        switch (variable) {
            .var_dec => |var_dec| {
                if (Strings.eqlIgnoreCase(var_dec.name, name)) {
                    return variable;
                }
            },
            .global_dec => {},
        }
    }
    return null;
}

fn calcVarOffset(self: *@This(), dataType: DataType) usize {
    var sum: usize = getVariableSize(dataType);
    for (self.variables.items) |variable| {
        sum += switch (variable) {
            .var_dec => |value| getVariableSize(value.type),
            .global_dec => 0,
        };
    }
    return sum;
}

fn calcGlobalOffset(self: *@This()) usize {
    var sum: usize = 0;
    for (self.variables.items) |variable| {
        sum += switch (variable) {
            .var_dec => 0,
            .global_dec => 1,
        };
    }
    return sum;
}

fn getVariableSize(value: DataType) usize {
    return switch (value) {
        .numeric => 8,
    };
}

pub fn init(allocator: std.mem.Allocator) @This() {
    return @This(){
        .allocator = allocator,
        .variables = VariableList.init(allocator),
        .operations = OperationList.init(allocator),
    };
}

pub fn compile(self: *@This(), path: []const u8, buffer: []const u8) !void {
    var lexer = Lexer.init(path, buffer);

    // Program
    _ = try getAndExpectId(&lexer, Lexer.TokenKind.Keyword, "PROGRAM");
    const program_name = try getAndExpect(&lexer, Lexer.TokenKind.Id);
    std.log.debug("Program name: {s}", .{program_name.token});
    _ = try getAndExpect(&lexer, Lexer.TokenKind.EndOfLine);

    while (lexer.next()) |current| {
        if (current.kind == TokenKind.Keyword and Strings.eqlIgnoreCase(current.token, "VAR")) {
            const var_name_token = try getAndExpect(&lexer, TokenKind.Id);
            const var_name = var_name_token.token;

            _ = try getAndExpect(&lexer, TokenKind.Colon);

            // TODO parse dataType
            _ = try getAndExpectId(&lexer, TokenKind.Keyword, "NUMERIC");
            _ = try getAndExpect(&lexer, TokenKind.EndOfLine);

            if (self.findVariable(var_name)) |_| {
                printError(var_name_token, "Variable '{s}' already declared\n", .{
                    var_name,
                });
                return error.UnexpectedToken;
            }

            const offset = self.calcVarOffset(DataType.numeric);
            const variable = Declaration{ .var_dec = .{
                .name = var_name,
                .offset = offset,
                .type = DataType.numeric,
            } };

            self.variables.append(variable) catch unreachable;
            continue;
            //
        } else if (current.kind == TokenKind.Keyword and Strings.eqlIgnoreCase(current.token, "BEGIN")) {
            _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
            try self.parseBody(&lexer);
            _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
        }
    }

    // _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
}

pub fn parsePrimary(self: *@This(), lexer: *Lexer) !?Arg {
    const token = lexer.next().?;
    const arg: Arg = switch (token.kind) {
        .Id => arg: {
            const declaration = self.findVariable(token.token) orelse {
                printError(token, "Variable '{s}' not defined \n", .{token.token});
                return error.UnexpectedToken;
            };
            break :arg Arg{ .variable = declaration.var_dec.offset };
        },
        .IntegerLiteral => arg: {
            const value = try token.asInteger();
            break :arg Arg{ .integerLiteral = value };
        },
        .StringLiteral => arg: {
            const dataOffset = self.calcGlobalOffset();
            const variable = Declaration{ .global_dec = .{
                .data = try token.asString(self.allocator),
                .address = dataOffset,
            } };
            self.variables.append(variable) catch unreachable;
            break :arg Arg{ .dataLiteral = dataOffset };
        },
        else => {
            printError(token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };

    return arg;
}

pub fn parseBody(self: *@This(), lexer: *Lexer) !void {
    while (lexer.next()) |current| {
        if (current.kind == TokenKind.Id) {
            const id = current.token;
            const peek_token = lexer.peek().?;

            // assign
            if (peek_token.kind == TokenKind.ColonEqual) {
                _ = try getAndExpect(lexer, TokenKind.ColonEqual);

                const declaration = self.findVariable(id) orelse {
                    printError(current, "Variable '{s}' not found\n", .{id});
                    return error.UnexpectedToken;
                };

                const opt_arg: ?Arg = try self.parsePrimary(lexer);
                if (opt_arg) |arg| {
                    _ = try getAndExpect(lexer, TokenKind.EndOfLine);
                    try self.operations.append(.{
                        .assign = .{
                            .offset = declaration.var_dec.offset,
                            .arg = arg,
                        },
                    });
                }
            } else if (peek_token.kind == TokenKind.OpenParent) {
                _ = try getAndExpect(lexer, TokenKind.OpenParent);

                // TODO: iterate arguments list
                var args = std.ArrayList(Arg).init(self.allocator);
                const opt_arg: ?Arg = try self.parsePrimary(lexer);
                if (opt_arg) |arg| {
                    try args.append(arg);
                }
                _ = try getAndExpect(lexer, TokenKind.CloseParent);
                _ = try getAndExpect(lexer, TokenKind.EndOfLine);

                try self.operations.append(.{ .call = .{
                    .name = id,
                    .args = try args.toOwnedSlice(),
                } });
            }
        } else if (current.kind == TokenKind.Keyword and Strings.eqlIgnoreCase(current.token, "END")) {
            break;
        } else {
            printError(current, "Unexpected token: {s}", .{current.token});
        }
    }
}

pub fn build(self: *@This()) !void {
    const file = try std.fs.cwd().createFile("output.asm", .{ .truncate = true });
    errdefer file.close();
    var buf = std.io.bufferedWriter(file.deprecatedWriter());
    var writer = buf.writer();

    {
        try writer.print("format ELF64\n", .{});
        try writer.print("section \".text\" executable align 8 \n", .{});
        try writer.print("public main\n", .{});
        try writer.print("main:\n", .{});

        try writer.print("  push rbp\n", .{});
        try writer.print("  mov rbp, rsp \n", .{});
        // try writer.print("  ;external funciton definition\n", .{});
        try writer.print("  extrn putchar\n", .{});
        try writer.print("  extrn printf\n", .{});

        // allocate stack
        for (self.variables.items) |variable| {
            switch (variable) {
                .var_dec => |value| {
                    const size: usize = getVariableSize(value.type);
                    // try writer.print("  ;stack allocation\n", .{});
                    try writer.print("  sub rsp, {d}    \n", .{size});
                },
                .global_dec => {},
            }
        }

        // TODO
        // RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, sono 64bit
        // EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, sono 32bit

        // assign value
        for (self.operations.items) |operation| {
            switch (operation) {
                .assign => |assign| {
                    const target = assign.offset;
                    switch (assign.arg) {
                        .variable => |arg_offset| {
                            // try writer.print("  ;assign stack var to another stack var\n", .{});
                            try writer.print("  mov rax, [rbp - {d}] \n", .{arg_offset});
                            try writer.print("  mov [rbp - {d}], rax \n", .{target});
                        },
                        .integerLiteral => |value| {
                            // try writer.print("  ;assign literal to stack var\n", .{});
                            try writer.print("  mov QWORD [rbp - {d}], {d} \n", .{ target, value });
                        },
                        .dataLiteral => |_| {
                            // TODO gestire il caso
                            @panic("Impossibile assgnare un data literal, ad esempio una stringa, ad una variabile");
                        },
                    }
                },
                .infix_plus => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    switch (infix_plus.lhs) {
                        .variable => |lhs_offset| try writer.print("  mov rax, [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try writer.print("  add rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try writer.print("  add rax, {d} \n", .{rhs_value}),
                        else => {},
                    }
                    try writer.print("  mov [rbp - {d}] rax\n", .{target_offset});
                },
                .call => |call| {
                    // try writer.print("  ;function call\n", .{});
                    const fnName = call.name;
                    const arg = call.args[0];
                    switch (arg) {
                        .variable => |offset| try writer.print("  mov rdi, [rbp - {d}] \n", .{offset}),
                        .integerLiteral => |value| try writer.print("  mov rdi, {d} \n", .{value}),
                        .dataLiteral => |value| try writer.print("  mov rdi, data{d}+0 \n", .{value}),
                    }

                    // TODO azzera il registo AL, serve per chiamara printf, non so se serve per tutte le funzioni extrn
                    try writer.print("  xor eax, eax\n", .{});

                    try writer.print("  call {s}\n", .{fnName});
                },
            }
        }

        // deallocate stack
        for (self.variables.items) |variable| {
            switch (variable) {
                .var_dec => |value| {
                    const size: usize = getVariableSize(value.type);
                    // try writer.print("  ;stack deallocation\n", .{});
                    try writer.print("  add rsp, {d}    \n", .{size});
                },
                .global_dec => {},
            }
        }
        // restore stack
        try writer.print("  pop rbp\n", .{});

        // try writer.print("  ;return status code\n", .{});
        try writer.print("  mov rax, 0\n", .{});
        try writer.print("  ret\n", .{});

        // data section
        try writer.print("section \".data\"\n", .{});
        for (self.variables.items) |variable| {
            switch (variable) {
                .var_dec => {},
                .global_dec => |global| {
                    try writer.print("   data{d}: db ", .{global.address});
                    for (global.data, 0..) |char, i| {
                        try writer.print("0x{X}", .{char});
                        if (i < global.data.len - 1) {
                            try writer.print(", ", .{});
                        } else {
                            try writer.print(", ", .{});
                        }
                    }
                    try writer.print("0x00", .{});
                    try writer.print("\n", .{});
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

pub fn expect(token: Token, kind: TokenKind) CompileError!void {
    if (token.kind != kind) {
        print("{s}:{d}:{d}: Expected {s}, got instead: {s} '{s}'\n", .{
            token.file_name,
            token.lineNumber,
            token.token_start,
            @tagName(kind),
            @tagName(token.kind),
            token.token,
        });
        return error.UnexpectedToken;
    }
}

pub fn getAndExpect(lexer: *Lexer, kind: TokenKind) CompileError!Token {
    const token = lexer.next() orelse {
        print("Unexpected EOF\n", .{});
        return error.UnexpectedToken;
    };
    try expect(token, kind);
    return token;
}

pub fn expectId(token: Token, kind: TokenKind, value: []const u8) CompileError!void {
    if (token.kind != kind or !Strings.eqlIgnoreCase(token.token, value)) {
        print("{s}:{d}:{d}: Expected Keyword: '{s}', got instead: '{s}'\n", .{
            token.file_name,
            token.lineNumber,
            token.token_start,
            value,
            token.token,
        });
        return error.UnexpectedToken;
    }
}

pub fn getAndExpectId(lexer: *Lexer, kind: TokenKind, value: []const u8) CompileError!Token {
    const token = lexer.next() orelse {
        print("Unexpected EOF\n", .{});
        return error.UnexpectedToken;
    };
    try expectId(token, kind, value);
    return token;
}

const CompileError = error{
    UnexpectedToken,
};

pub fn printError(token: Token, comptime fmt: []const u8, args: anytype) void {
    print("{s}:{d}:{d} L3P Compilation Error\n", .{
        token.file_name,
        token.lineNumber,
        token.token_start,
    });
    print(fmt, args);
}

pub fn print(comptime format: []const u8, args: anytype) void {
    const stdout_file = std.fs.File.stdout().deprecatedWriter();
    stdout_file.print(format, args) catch unreachable;
}
