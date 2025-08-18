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
const OpType = enum { label, jump, jump_if_false, assign, infix_plus, infix_minus, infix_multiply, infix_divide, infix_less, call };
const Op = union(OpType) {
    label: usize,
    jump: usize,
    jump_if_false: struct {
        label: usize,
        arg: Arg,
    },
    assign: struct {
        offset: usize,
        arg: Arg,
    },
    infix_plus: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    infix_minus: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    infix_multiply: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    infix_divide: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    infix_less: struct {
        offset: usize,
        lhs: Arg,
        rhs: Arg,
    },
    call: struct {
        name: []const u8,
        args: []const Arg,
    },
};
const OperationList = std.ArrayList(Op);

allocator: std.mem.Allocator,
variables: VariableList,
operations: OperationList,
jump_labels: usize = 0,

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
    _ = try getAndExpect(&lexer, Lexer.TokenKind.Id);
    // std.log.debug("Program name: {s}", .{program_name.token});
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
            // std.debug.print("BEGIN\n", .{});
            try self.parseBody(&lexer);
            _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
        }
    }

    // _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
}

pub fn parsePrimary(self: *@This(), lexer: *Lexer) !Arg {
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
            try self.variables.append(variable);
            break :arg Arg{ .dataLiteral = dataOffset };
        },
        else => {
            printError(token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };
    return arg;
}

fn isOperator(k: TokenKind) bool {
    return switch (k) {
        .Plus, .Minus, .Multiply, .Divide, .Less, .LessEqual, .Greater, .GreaterEqual => true,
        else => false,
    };
}

fn getPrecedence(k: TokenKind) usize {
    return switch (k) {
        .Plus, .Minus => 1,
        .Multiply, .Divide => 2,
        .Less, .LessEqual, .Greater, .GreaterEqual => 3,
        else => 0,
    };
}

pub fn compileExpression(self: *@This(), lexer: *Lexer) !Arg {
    const lhs = try self.parsePrimary(lexer);
    const res = try self.compileExpressionRecursive(lexer, lhs, 0);
    // std.debug.print("{} \n", .{res});
    return res;
}

pub fn compileExpressionRecursive(self: *@This(), lexer: *Lexer, arg: Arg, precedence: usize) !Arg {
    var lookahead = lexer.peek().?;
    var lhs = arg;
    while (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) >= precedence) {
        _ = lexer.next().?;
        const op: TokenKind = lookahead.kind;
        var rhs = try self.parsePrimary(lexer);

        lookahead = lexer.peek().?;
        if (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) > getPrecedence(op)) {
            rhs = try self.compileExpressionRecursive(lexer, rhs, getPrecedence(op) + 1);
            lookahead = lexer.peek().?;
        }

        const address = self.calcVarOffset(DataType.numeric);
        const temp_var = Declaration{
            .var_dec = .{
                .name = "",
                .offset = address,
                .type = DataType.numeric,
            },
        };

        try self.variables.append(temp_var);
        switch (op) {
            .Plus => try self.operations.append(.{
                .infix_plus = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Minus => try self.operations.append(.{
                .infix_minus = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Multiply => try self.operations.append(.{
                .infix_multiply = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Divide => try self.operations.append(.{
                .infix_divide = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Less => try self.operations.append(.{
                .infix_less = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            else => @panic("Not implemented"),
        }
        lhs = Arg{ .variable = address };
    }
    return lhs;
}

pub fn parseBody(self: *@This(), lexer: *Lexer) !void {
    while (lexer.next()) |current| {
        // std.debug.print("{any}", .{current});

        if (current.kind == TokenKind.Keyword and Strings.eqlIgnoreCase(current.token, "WHILE")) {
            // Create label before while expression

            const inner_label = self.jump_labels;
            try self.operations.append(.{ .label = inner_label });

            // while boolean expression
            const arg = try self.compileExpression(lexer);

            // Jump to label after while
            self.jump_labels += 1;
            const after_while_label = self.jump_labels;
            const jump_if = Op{ .jump_if_false = .{ .label = after_while_label, .arg = arg } };
            try self.operations.append(jump_if);

            _ = try getAndExpectId(lexer, TokenKind.Keyword, "DO"); // DO
            _ = try getAndExpect(lexer, TokenKind.EndOfLine);

            _ = try getAndExpectId(lexer, TokenKind.Keyword, "BEGIN"); // BEGIN
            _ = try getAndExpect(lexer, TokenKind.EndOfLine);

            try self.parseBody(lexer);

            _ = try getAndExpect(lexer, TokenKind.EndOfLine);

            _ = try getAndExpectId(lexer, TokenKind.Keyword, "ENDWHILE"); // BEGIN
            _ = try getAndExpect(lexer, TokenKind.EndOfLine);

            // Jump to start of while
            try self.operations.append(.{ .jump = inner_label });
            // Create label after while
            try self.operations.append(.{ .label = after_while_label });
        } else if (current.kind == TokenKind.Id) {
            const id = current.token;
            const peek_token = lexer.peek().?;

            // assign
            if (peek_token.kind == TokenKind.ColonEqual) {
                _ = try getAndExpect(lexer, TokenKind.ColonEqual);

                const declaration = self.findVariable(id) orelse {
                    printError(current, "Variable '{s}' not found\n", .{id});
                    return error.UnexpectedToken;
                };

                const arg = try self.compileExpression(lexer);

                _ = try getAndExpect(lexer, TokenKind.EndOfLine);
                try self.operations.append(.{
                    .assign = .{
                        .offset = declaration.var_dec.offset,
                        .arg = arg,
                    },
                });
            } else if (peek_token.kind == TokenKind.OpenParent) {
                _ = try getAndExpect(lexer, TokenKind.OpenParent);

                // TODO: iterate arguments list
                var args = std.ArrayList(Arg).init(self.allocator);

                while (lexer.peek().?.kind != TokenKind.CloseParent) {
                    const arg = try self.compileExpression(lexer);
                    try args.append(arg);

                    if (lexer.peek().?.kind == TokenKind.Comma) {
                        _ = lexer.next();
                    }
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
                .label => |index| {
                    try writer.print("label_{d}: \n", .{index});
                },
                .jump => |index| {
                    try writer.print("  jmp label_{d}\n", .{index});
                },
                .jump_if_false => |jump_if_false| {
                    const target = jump_if_false.label;
                    const arg = jump_if_false.arg;
                    switch (arg) {
                        .variable => |lhs_offset| try writer.print("  mov rax, [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    try writer.print("  cmp rax, 0\n", .{});
                    try writer.print("  je label_{d}\n", .{target});
                },
                .assign => |assign| {
                    const target = assign.offset;
                    switch (assign.arg) {
                        .variable => |arg_offset| {
                            // try writer.print("  ;assign stack var to another stack var\n", .{});
                            try writer.print("  mov rax, QWORD [rbp - {d}] \n", .{arg_offset});
                            try writer.print("  mov QWORD [rbp - {d}], rax \n", .{target});
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
                    try writer.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_minus => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    switch (infix_plus.lhs) {
                        .variable => |lhs_offset| try writer.print("  mov rax, QWORD [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov QWORD rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try writer.print("  sub rax, QWORD [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try writer.print("  sub QWORD rax, {d} \n", .{rhs_value}),
                        else => {},
                    }
                    try writer.print("  mov QWORD [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_multiply => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    switch (infix_plus.lhs) {
                        .variable => |lhs_offset| try writer.print("  mov rax, [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try writer.print("  imul rax, [rbp - {d}] \n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try writer.print("  imul rax, {d} \n", .{rhs_value}),
                        else => {},
                    }
                    try writer.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_divide => |infix_plus| {
                    const target_offset = infix_plus.offset;
                    // Clear RDX
                    try writer.print("  xor rdx, rdx\n", .{});
                    switch (infix_plus.lhs) {
                        .variable => |lhs_offset| try writer.print("  mov rax, [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    switch (infix_plus.rhs) {
                        .variable => |rhs_offset| try writer.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try writer.print("  mov rbx, {d} \n", .{rhs_value}),
                        else => {},
                    }
                    try writer.print("  div rbx\n", .{});
                    // ; RAX = RAX / RBX
                    // , RDX = RAX % RBX
                    try writer.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .infix_less => |infix_less| {
                    const target_offset = infix_less.offset;
                    switch (infix_less.lhs) {
                        .variable => |lhs_offset| try writer.print("  mov rax, [rbp - {d}] \n", .{lhs_offset}),
                        .integerLiteral => |lhs_value| try writer.print("  mov rax, {d} \n", .{lhs_value}),
                        else => {},
                    }
                    switch (infix_less.rhs) {
                        .variable => |rhs_offset| try writer.print("  mov rbx, [rbp - {d}]\n", .{rhs_offset}),
                        .integerLiteral => |rhs_value| try writer.print("  mov rbx, {d} \n", .{rhs_value}),
                        else => {},
                    }
                    try writer.print("  cmp rax, rbx\n", .{});
                    try writer.print("  setl al\n", .{});
                    try writer.print("  mov [rbp - {d}], rax\n", .{target_offset});
                },
                .call => |call| {
                    // try writer.print("  ;function call\n", .{});
                    const fnName = call.name;
                    const registers = [_][]const u8{ "RDI", "RSI", "RDX", "RCX", "R8", "R9" };
                    if (call.args.len <= registers.len) {
                        for (call.args, 0..) |arg, i| {
                            const reg = registers[i];
                            switch (arg) {
                                .variable => |offset| try writer.print("  mov {s}, [rbp - {d}] \n", .{ reg, offset }),
                                .integerLiteral => |value| try writer.print("  mov {s}, {d} \n", .{ reg, value }),
                                .dataLiteral => |value| try writer.print("  mov {s}, data{d}+0 \n", .{ reg, value }),
                            }
                        }
                        // TODO azzera il registo AL, serve per chiamara printf, non so se serve per tutte le funzioni extrn
                        try writer.print("  xor eax, eax\n", .{});
                        try writer.print("  call {s}\n", .{fnName});
                    } else {
                        @panic("Call a funzioni esterne supporta al massimo 6 argomenti");
                    }
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
