const std = @import("std");
const Strings = @import("Strings.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

const DataType = enum { numeric };
const InstructionType = enum { var_dec, assign };
const Instruction = union(InstructionType) {
    var_dec: struct {
        address: usize,
        type: DataType,
    },
    assign: i32,
};

const VariableList = std.ArrayList(Instruction);
const OperationList = std.ArrayList(Instruction);

allocator: std.mem.Allocator,
variables: VariableList,
operations: OperationList,

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

    // Variables
    {
        _ = try getAndExpectId(&lexer, Lexer.TokenKind.Keyword, "VAR");
        const var1_declaration = try getAndExpect(&lexer, TokenKind.Id);
        _ = var1_declaration;
        _ = try getAndExpect(&lexer, TokenKind.Colon);
        const var1_type = try getAndExpectId(&lexer, TokenKind.Keyword, "NUMERIC");
        _ = var1_type;
        _ = try getAndExpect(&lexer, TokenKind.EndOfLine);

        const variable = Instruction{ .var_dec = .{ .address = 0, .type = DataType.numeric } };
        self.variables.append(variable) catch unreachable;
    }

    // Bodye
    _ = try getAndExpectId(&lexer, TokenKind.Keyword, "BEGIN");
    _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
    {
        const variable = try getAndExpect(&lexer, TokenKind.Id);
        _ = variable;
        _ = try getAndExpect(&lexer, TokenKind.ColonEqual);
        const integerLiteral = try getAndExpect(&lexer, TokenKind.IntegerLiteral);
        _ = try getAndExpect(&lexer, TokenKind.EndOfLine);

        const value = try integerLiteral.asInteger();
        try self.operations.append(.{ .assign = value });
    }

    // _ = try getAndExpectId(&lexer, TokenKind.Keyword, "END");
    // _ = try getAndExpect(&lexer, TokenKind.EndOfLine);
}

pub fn build(self: *@This()) !void {
    const file = try std.fs.cwd().createFile("output.asm", .{ .truncate = true });
    errdefer file.close();
    var buf = std.io.bufferedWriter(file.deprecatedWriter());
    var writer = buf.writer();

    {
        try writer.print("format ELF64\n", .{});
        try writer.print("section '.text' executable\n", .{});
        try writer.print("public main\n", .{});
        try writer.print("main:                \n", .{});
        try writer.print("  extrn putchar      \n", .{});

        // allocate stack
        try writer.print("  sub rsp, 32         \n", .{});

        for (self.operations.items) |operation| {
            switch (operation) {
                .assign => |value| {
                    try writer.print("  mov rdi, {d}    \n", .{value});
                },
                .var_dec => {},
            }
        }

        try writer.print("  call putchar       \n", .{});

        // deallocate stack
        try writer.print("  add rsp, 32         \n", .{});

        try writer.print("  mov rax, 0         \n", .{}); // ret code 0
        try writer.print("  ret                \n", .{});
    }
    try buf.flush();
    file.close();

    {
        var cmd = std.process.Child.init(&[_][]const u8{ "fasm", "output.asm" }, self.allocator);
        try cmd.spawn();
        _ = try cmd.wait();
    }

    {
        var cmd = std.process.Child.init(&[_][]const u8{ "gcc", "-no-pie", "-lc", "-o", "output", "output.o" }, self.allocator);
        try cmd.spawn();
        _ = try cmd.wait();
    }
}

pub fn getAndExpect(lexer: *Lexer, kind: TokenKind) CompileError!Token {
    const token = lexer.next() orelse {
        print("Unexpected EOF\n", .{});
        return error.UnexpectedToken;
    };
    if (token.kind != kind) {
        print("{s}:{d}:{d}: Expected {s}, got instead: {s} '{s}'\n", .{
            lexer.file_name,
            token.lineNumber,
            token.token_start,
            @tagName(kind),
            @tagName(token.kind),
            token.token,
        });
        return error.UnexpectedToken;
    }
    return token;
}

pub fn getAndExpectId(lexer: *Lexer, kind: TokenKind, value: []const u8) CompileError!Token {
    const token = lexer.next() orelse {
        print("Unexpected EOF\n", .{});
        return error.UnexpectedToken;
    };
    if (token.kind != kind or !Strings.eqlIgnoreCase(token.token, value)) {
        print("{s}:{d}:{d}: Expected Keyword: '{s}', got instead: '{s}'\n", .{
            lexer.file_name,
            token.lineNumber,
            token.token_start,
            value,
            token.token,
        });
        return error.UnexpectedToken;
    }
    return token;
}

const CompileError = error{
    UnexpectedToken,
};

pub fn print(comptime format: []const u8, args: anytype) void {
    const stdout_file = std.fs.File.stdout().deprecatedWriter();
    stdout_file.print(format, args) catch unreachable;
}
