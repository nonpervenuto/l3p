const std = @import("std");
const Strings = @import("Strings.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

const ArgType = enum { argOffset, argLiteral };
const Arg = union(ArgType) { argOffset: usize, argLiteral: i32 };

const DataType = enum { numeric };
const DeclarationType = enum { var_dec };
const Declaration = union(DeclarationType) {
    var_dec: struct {
        name: []const u8,
        offset: usize,
        type: DataType,
    },
};
const VariableList = std.ArrayList(Declaration);

// expressions
const InstructionType = enum { assign, call };
const Instruction = union(InstructionType) { assign: struct {
    offset: usize,
    value: i32,
}, call: struct {
    name: []const u8,
    args: []const Arg,
} };
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
        }
    }
    return null;
}

fn calcOffset(self: *@This(), dataType: DataType) usize {
    var sum: usize = 0;
    for (self.variables.items) |variable| {
        sum += switch (variable) {
            .var_dec => |value| value.offset,
        };
    }
    return sum + getVariableSize(dataType);
}

fn getVariableSize(value: DataType) usize {
    return switch (value) {
        .numeric => 32,
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

            const offset = self.calcOffset(DataType.numeric);
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

pub fn parseBody(self: *@This(), lexer: *Lexer) !void {
    while (lexer.next()) |current| {
        if (current.kind == TokenKind.Id) {
            const id = current.token;
            const peek_token = lexer.peek().?;

            // assign
            if (peek_token.kind == TokenKind.ColonEqual) {
                _ = try getAndExpect(lexer, TokenKind.ColonEqual);
                const integerLiteral = try getAndExpect(lexer, TokenKind.IntegerLiteral);
                const value = try integerLiteral.asInteger();
                _ = try getAndExpect(lexer, TokenKind.EndOfLine);

                const declaration = self.findVariable(id) orelse {
                    printError(current, "Variable '{s}' not found\n", .{id});
                    return error.UnexpectedToken;
                };

                try self.operations.append(.{ .assign = .{
                    .offset = declaration.var_dec.offset,
                    .value = value,
                } });
            } else if (peek_token.kind == TokenKind.OpenParent) {
                _ = try getAndExpect(lexer, TokenKind.OpenParent);

                const firstArgument = lexer.next().?;

                var args = std.ArrayList(Arg).init(self.allocator);

                switch (firstArgument.kind) {
                    .Id => {
                        const declaration = self.findVariable(firstArgument.token) orelse {
                            printError(current, "Variable '{s}' not defined \n", .{firstArgument.token});
                            return error.UnexpectedToken;
                        };
                        try args.append(.{ .argOffset = declaration.var_dec.offset });
                    },
                    .IntegerLiteral => {
                        const value = try firstArgument.asInteger();
                        try args.append(.{ .argLiteral = value });
                    },
                    else => {
                        printError(current, "Unexpected token '{s}' in function call {s} \n", .{ firstArgument.token, id });
                        return error.UnexpectedToken;
                    },
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
        try writer.print("section \".text\" executable\n", .{});
        try writer.print("public main\n", .{});
        try writer.print("main:\n", .{});

        try writer.print("  push rbp\n", .{});
        try writer.print("  mov rbp, rsp \n", .{});
        try writer.print("  extrn putchar\n", .{});

        // allocate stack
        for (self.variables.items) |variable| {
            switch (variable) {
                .var_dec => |value| {
                    const size: usize = getVariableSize(value.type);
                    try writer.print("  sub rsp, {d}    \n", .{size});
                },
            }
        }

        // assign value
        for (self.operations.items) |operation| {
            switch (operation) {
                .assign => |assign| {
                    try writer.print("  mov [rbp - {d}], DWORD {d}    \n", .{ assign.offset, assign.value });
                },
                .call => |call| {
                    const fnName = call.name;
                    const arg = call.args[0];
                    switch (arg) {
                        .argOffset => |offset| try writer.print("  mov rdi, [rbp - {d}] \n", .{offset}),
                        .argLiteral => |value| try writer.print("  mov rdi, {d} \n", .{value}),
                    }
                    try writer.print("  call {s}\n", .{fnName});
                },
            }
        }

        // deallocate stack
        for (self.variables.items) |variable| {
            switch (variable) {
                .var_dec => |value| {
                    const size: usize = getVariableSize(value.type);
                    try writer.print("  add rsp, {d}    \n", .{size});
                },
            }
        }

        // restore stack
        try writer.print("  pop rbp\n", .{});

        // exit
        try writer.print("  mov rax, 60  \n", .{});
        try writer.print("  mov rdi, 0   \n", .{});
        try writer.print("  int 0x80     \n", .{});
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
