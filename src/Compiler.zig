const std = @import("std");

const Ir = @import("Ir.zig");
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

const Self = @This();

pub const CompileError = error{
    OutOfMemory,
    InvalidLiteral,
    UnexpectedToken,
    UnexpectedEOF,
    PrintDiagnosticError,
};

allocator: std.mem.Allocator,
ir: Ir,
pub fn init(allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .ir = .{
            .variables = .empty,
            .operations = .empty,
        },
    };
}

/// Compile the provided program and return the generated IR
pub fn compile(self: *Self, path: []const u8, buffer: []const u8) !Ir {
    return try self.compileProgram(path, buffer);
}

/// Compile the root program
pub fn compileProgram(self: *Self, path: []const u8, buffer: []const u8) !Ir {
    var lexer = Lexer.init(path, buffer);
    try fetchExpectMany(&lexer, .{ .Program, .Identifier, .EndOfLine });
    try self.compileVars(&lexer);
    try fetchExpectMany(&lexer, .{ .Begin, .EndOfLine });
    try self.parseBody(&lexer, .{.End});
    try fetchExpectMany(&lexer, .{ .End, .EndOfLine });

    return self.ir;
}

fn compileVars(self: *Self, lexer: *Lexer) !void {
    while (lexer.peek().?.kind == TokenKind.Var) {
        var vars: std.ArrayList([]const u8) = .empty;

        // Parse variable name list
        try fetchExpectMany(lexer, .{.Var});
        while (lexer.peek().?.kind != TokenKind.Colon) {
            const var_token = try getExpect(lexer, .Identifier);
            try vars.append(self.allocator, var_token.token);

            if (lexer.peek().?.kind == TokenKind.Comma) {
                try fetchExpectMany(lexer, .{.Comma});
            }
        }

        // Parse data types
        try fetchExpectMany(lexer, .{.Colon});

        const token = lexer.next();
        const kind = token.?.kind;
        if (kind == TokenKind.Numeric) {
            try fetchExpectMany(lexer, .{.EndOfLine});
            for (vars.items) |var_name| {
                const offset = self.ir.calcVarOffset(Ir.DataType.numeric);
                const variable = Ir.Declaration{ .var_dec = .{
                    .name = var_name,
                    .offset = offset,
                    .type = Ir.DataType.numeric,
                } };
                self.ir.variables.append(self.allocator, variable) catch unreachable;
            }
        } else if (kind == TokenKind.Array) {
            try fetchExpectMany(lexer, .{.OpenSquare});
            const token_array_size = try getExpect(lexer, .IntegerLiteral);
            const array_size = token_array_size.asUsize();
            _ = array_size;
            try fetchExpectMany(lexer, .{ .CloseSquare, .Of, .Numeric, .EndOfLine });

            for (vars.items) |var_name| {
                const dataOffset = self.ir.calcGlobalOffset();
                const variable = Ir.Declaration{
                    .global_dec = .{
                        .name = var_name,
                        .address = dataOffset,
                        .data = null,
                    },
                };
                try self.ir.variables.append(self.allocator, variable);
            }
        } else {
            try diagnostic(lexer, token, "Expected a type for var but none was found\n", .{});
            return CompileError.UnexpectedToken;
        }
    }
}

fn isOperator(k: TokenKind) bool {
    return switch (k) {
        .Assign,
        .Or,
        .And,
        .Pipe,
        .Hat,
        .Ampersand,
        .Equal,
        .ExclamationEqual,
        .Less,
        .LessEqual,
        .Greater,
        .GreaterEqual,
        .LessLess,
        .GreaterGreater,
        .Plus,
        .Minus,
        .Multiply,
        .Divide,
        .Percent,
        .Not,
        => true,
        else => false,
    };
}

// Operation precedence.
// the greater the number, the more precedence it has and must be executed first
fn getPrecedence(k: TokenKind) usize {
    return switch (k) {
        .Assign => 0,
        // TODO Operatore virgola
        // TODO = <<= >>=
        .Or => 1, // OR logico
        .And => 2, // AND logico
        .Pipe => 3, // OR
        .Hat => 4, // XOR
        .Ampersand => 5, // AND bit a bit
        .Equal, .ExclamationEqual => 6, // Operatori di uguaglianza
        .Less, .LessEqual, .Greater, .GreaterEqual => 7, // Operatori relazionali
        .LessLess, .GreaterGreater => 8, // postamento bit a sinistra e a destra
        .Plus, .Minus => 9, // Addizione, sottrazione
        .Multiply, .Divide, .Percent => 10, // Moltiplicazione, divisione, modulo
        .Not => 11, // Operatori unari, sizeof, cast di tipo
        else => @panic("Unexpected Token"),
    };
}

pub fn compileExpression(self: *Self, lexer: *Lexer) CompileError!Ir.Arg {
    const lhs = try self.parsePrimary(lexer);
    return try self.compileExpressionRecursive(lexer, lhs, 0);
}

pub fn parsePrimary(self: *Self, lexer: *Lexer) CompileError!Ir.Arg {
    const token = lexer.next().?;
    std.log.debug("Parsing: {}", .{token.kind});
    const arg: Ir.Arg = switch (token.kind) {
        // Var or Function call
        .Identifier => arg: {
            const isPeekOpenParent = if (lexer.peek()) |peek| peek.kind == TokenKind.OpenParent else false;
            if (isPeekOpenParent) {
                try fetchExpectMany(lexer, .{.OpenParent});
                const ArgList = std.ArrayList(Ir.Arg);
                var args: ArgList = .empty;

                while (lexer.peek().?.kind != TokenKind.CloseParent) {
                    const arg = try self.compileExpression(lexer);
                    try args.append(self.allocator, arg);

                    if (lexer.peek().?.kind == TokenKind.Comma) {
                        _ = lexer.next();
                    }
                }
                try fetchExpectMany(lexer, .{.CloseParent});

                // The result of the function is stored in a temporary variable
                const address = try self.ir.createTempVar(self.allocator, Ir.DataType.numeric);
                try self.ir.operations.append(self.allocator, .{ .call = .{
                    .name = token.token,
                    .offset = address,
                    .args = try args.toOwnedSlice(self.allocator),
                } });

                break :arg Ir.Arg{ .variable = address };
            } else {
                const variable = self.ir.findVariable(token.token) orelse {
                    try diagnostic(lexer, token, "Variable '{s}' is not defined.\n", .{token.token});
                    return error.UnexpectedToken;
                };
                break :arg Ir.Arg{ .variable = variable.var_dec.offset };
            }
        },
        .IntegerLiteral => arg: {
            const value = token.asInteger() catch {
                try diagnostic(lexer, token, "Invalid number literal '{s}'.\n", .{token.token});
                return error.UnexpectedToken;
            };
            break :arg Ir.Arg{ .integerLiteral = value };
        },
        .StringLiteral => arg: {
            const dataOffset = self.ir.calcGlobalOffset();
            const variable = Ir.Declaration{ .global_dec = .{
                .name = "data",
                .data = try token.asString(self.allocator),
                .address = dataOffset,
            } };
            try self.ir.variables.append(self.allocator, variable);
            break :arg Ir.Arg{ .dataLiteral = dataOffset };
        },
        .OpenParent => arg: {
            const lhs = try self.compileExpression(lexer);
            try fetchExpectMany(lexer, .{.CloseParent});
            break :arg lhs;
        },
        // Deref
        .Ampersand => arg: {
            const arg = try self.compileExpression(lexer);
            const address = try self.ir.createTempVar(self.allocator, Ir.DataType.numeric);
            try self.ir.operations.append(self.allocator, .{
                .ref = .{ .offset = address, .arg = arg },
            });
            break :arg Ir.Arg{ .variable = address };
        },
        .Multiply => arg: {
            const arg = try self.parsePrimary(lexer);
            break :arg Ir.Arg{ .deref = arg.variable };
        },
        // Unary Not
        .Exclamation => arg: {
            const arg = try self.compileExpression(lexer);
            const address = try self.ir.createTempVar(self.allocator, Ir.DataType.numeric);
            try self.ir.operations.append(self.allocator, .{
                .unary_not = .{ .offset = address, .arg = arg },
            });
            break :arg Ir.Arg{ .variable = address };
        },
        .Minus => arg: {
            const arg = try self.parsePrimary(lexer);
            switch (arg) {
                .integerLiteral => |value| {
                    break :arg Ir.Arg{ .integerLiteral = -value };
                },
                else => {
                    const address = try self.ir.createTempVar(self.allocator, Ir.DataType.numeric);
                    try self.ir.operations.append(self.allocator, .{
                        .unary_neg = .{ .offset = address, .arg = arg },
                    });
                    break :arg Ir.Arg{ .variable = address };
                },
            }
        },
        else => {
            try diagnostic(lexer, token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };
    return arg;
}

pub fn compileExpressionRecursive(self: *Self, lexer: *Lexer, arg: Ir.Arg, precedence: usize) CompileError!Ir.Arg {
    var lookahead = lexer.peek() orelse return error.UnexpectedEOF;
    var lhs = arg;
    while (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) >= precedence) {
        _ = lexer.next() orelse return error.UnexpectedEOF;
        const kind: TokenKind = lookahead.kind;
        var rhs = try self.parsePrimary(lexer);

        lookahead = lexer.peek() orelse return error.UnexpectedEOF;
        if (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) > getPrecedence(kind)) {
            rhs = try self.compileExpressionRecursive(lexer, rhs, getPrecedence(kind) + 1);
            lookahead = lexer.peek() orelse return error.UnexpectedEOF;
        }

        // TODO if kind is assigment this temporary varialbe is not necessary
        const address = try self.ir.createTempVar(self.allocator, Ir.DataType.numeric);

        const op: Ir.Op = switch (kind) {
            .Assign => .{ .assign = .{ .lhs = lhs, .rhs = rhs } },
            .Or => .{ .infix_or = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .And => .{ .infix_and = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Pipe => .{ .infix_bit_or = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Hat => .{ .infix_bit_xor = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Ampersand => .{ .infix_bit_and = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Equal => .{ .infix_eq = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .ExclamationEqual => .{ .infix_not_eq = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Less => .{ .infix_l = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .LessEqual => .{ .infix_le = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .GreaterEqual => .{ .infix_ge = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Greater => .{ .infix_g = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .LessLess => .{ .infix_shift_left = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .GreaterGreater => .{ .infix_shift_right = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Plus => .{ .infix_plus = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Minus => .{ .infix_minus = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Multiply => .{ .infix_multiply = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Divide => blk: {
                if (rhs.integerLiteral == 0) {
                    try diagnostic(lexer, lookahead, "Division by zero detected!\n", .{});
                    return error.UnexpectedToken;
                }
                break :blk .{ .infix_divide = .{ .offset = address, .lhs = lhs, .rhs = rhs } };
            },
            .Percent => .{ .infix_modulo = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            else => @panic("Not implemented"),
        };
        try self.ir.operations.append(self.allocator, op);

        // TODO not every operation return an address, see asigment
        lhs = Ir.Arg{ .variable = address };
    }
    return lhs;
}

pub fn parseBody(self: *Self, lexer: *Lexer, close_body_tags: anytype) CompileError!void {
    while (lexer.peek()) |peek| {
        if (peek.kind == TokenKind.If) {
            try fetchExpectMany(lexer, .{.If});

            // if boolean expression
            var arg = try self.compileExpression(lexer);
            try fetchExpectMany(lexer, .{ .Then, .EndOfLine });

            const end_if = try self.ir.createLabel(self.allocator, "END_IF");
            var jump_forward = try self.ir.createLabel(self.allocator, "IF_COND_FALSE");

            try self.ir.operations.append(self.allocator, Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
            try self.parseBody(lexer, .{ .Else, .ElseIf, .EndIf });
            try self.ir.operations.append(self.allocator, Ir.Op{ .jump = end_if });
            var nextToken = lexer.peek() orelse return error.UnexpectedEOF;

            while (nextToken.kind == .ElseIf) {
                try fetchExpectMany(lexer, .{.ElseIf});
                try self.ir.operations.append(self.allocator, .{ .label = jump_forward });
                jump_forward = try self.ir.createLabel(self.allocator, "ELSE_IF_COND_FALSE");
                arg = try self.compileExpression(lexer);

                // Consume Then
                try fetchExpectMany(lexer, .{ .Then, .EndOfLine });
                try self.ir.operations.append(self.allocator, Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
                try self.parseBody(lexer, .{ .ElseIf, .Else, .EndIf });
                try self.ir.operations.append(self.allocator, Ir.Op{ .jump = end_if });

                nextToken = lexer.peek() orelse return error.UnexpectedEOF;
            }

            if (nextToken.kind == .Else) {
                try fetchExpectMany(lexer, .{ .Else, .EndOfLine });
                try self.ir.operations.append(self.allocator, .{ .label = jump_forward });
                try self.parseBody(lexer, .{.EndIf});
            } else {
                try self.ir.operations.append(self.allocator, .{ .label = jump_forward });
            }
            try fetchExpectMany(lexer, .{ .EndIf, .EndOfLine });
            try self.ir.operations.append(self.allocator, .{ .label = end_if });
        } else if (peek.kind == TokenKind.While) {
            try fetchExpectMany(lexer, .{.While});

            // Create label before while expression
            const while_loop = try self.ir.createLabel(self.allocator, "WHILE");
            try self.ir.operations.append(self.allocator, .{ .label = while_loop });

            // while boolean expression
            const arg = try self.compileExpression(lexer);

            // Jump to label after while
            const while_end = try self.ir.createLabel(self.allocator, "END_WHILE");
            const jump_if = Ir.Op{ .jump_if_false = .{ .label = while_end, .arg = arg } };
            try self.ir.operations.append(self.allocator, jump_if);

            try fetchExpectMany(lexer, .{ .Do, .EndOfLine });
            try self.parseBody(lexer, .{.EndWhile});
            try fetchExpectMany(lexer, .{ .EndWhile, .EndOfLine });

            // Jump to start of while
            try self.ir.operations.append(self.allocator, .{ .jump = while_loop });
            // Create label after while
            try self.ir.operations.append(self.allocator, .{ .label = while_end });
        } else if (peek.kind == TokenKind.Identifier or peek.kind == TokenKind.Multiply) {
            _ = try self.compileExpression(lexer);
            try fetchExpectMany(lexer, .{.EndOfLine});
        } else {
            // Each body can close with a different tag.
            // For example 'while' must be closed with 'endwhile',
            // 'if' can be closed with an 'elseif' or an 'else' or an 'endif'
            const fields = std.meta.fields(@TypeOf(close_body_tags));
            inline for (fields) |field| {
                if (peek.kind == @field(close_body_tags, field.name)) {
                    return;
                }
            }
            try diagnostic(lexer, peek, "Unexpected token: {s}", .{peek.token});
            return;
        }
    }
}

/// Check the token without consuming
pub fn expect(lexer: *Lexer, token: ?Token, kind: TokenKind) CompileError!void {
    const t = token orelse {
        diagnostic(lexer, null, "Expected {s}, got instead 'end of file' \n", .{
            @tagName(kind),
        }) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    };
    if (t.kind != kind) {
        diagnostic(lexer, t, "Expected keyword '{s}', got instead: {s} '{s}'\n", .{
            @tagName(kind),
            @tagName(t.kind),
            t.token,
        }) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    }
}

// Consume the next token and check for the expected kind, and return the token
pub fn getExpect(lexer: *Lexer, comptime kind: TokenKind) !Token {
    const token = lexer.next();
    try expect(lexer, token, kind);
    if (token) |t| return t else unreachable;
}

/// Consume the next token and check for the expected kind in the list
pub fn fetchExpectMany(lexer: *Lexer, comptime kinds: anytype) !void {
    const fields = std.meta.fields(@TypeOf(kinds));
    inline for (fields) |field| {
        try expect(lexer, lexer.next(), @field(kinds, field.name));
    }
}

pub fn diagnostic(lexer: *Lexer, token: ?Token, comptime fmt: []const u8, args: anytype) !void {
    var writer = std.fs.File.stderr().writer(&.{});
    const std_err = &writer.interface;
    const t = token orelse {
        std_err.print("Unexpected 'end of file'\nMake sure to balance every 'BEGIN' with and 'END'\n", .{}) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    };
    const loc = lexer.getLoc(t);
    std_err.print("{s}:{d}:{d} : " ++ "\x1b[31m" ++ "L3P Compilation Error " ++ "\x1b[0m", .{ t.file_name, loc.row, loc.col }) catch return error.PrintDiagnosticError;
    std_err.print(fmt, args) catch return error.PrintDiagnosticError;
    std_err.flush() catch return error.PrintDiagnosticError;
}
