const std = @import("std");

const System = @import("System.zig");
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
            .globals = .empty,
            .body = .{ .variables = .empty, .operations = .empty },
            .functions = .empty,
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

    // skip new lines at the start of the file
    while (try accept(&lexer, lexer.peek(), .EndOfLine)) {
        _ = lexer.next();
    }

    try fetchExpectMany(&lexer, .{ .Program, .Identifier, .EndOfLine });
    try self.compileVars(&lexer, &self.ir.body);

    // Check if the next token is Function or Procedure
    while (try peekAny(&lexer, .{ .Function, .Procedure })) {
        if (try accept(&lexer, lexer.peek(), .Function)) {
            try self.compileFunction(&lexer);
        } else if (try accept(&lexer, lexer.peek(), .Procedure)) {
            try self.compileProcedure(&lexer);
        }
    }

    try fetchExpectMany(&lexer, .{ .Begin, .EndOfLine });
    try self.parseBody(&lexer, &self.ir.body, .{.End});
    try fetchExpectMany(&lexer, .{ .End, .EndOfLine });

    return self.ir;
}

fn compileVars(self: *Self, lexer: *Lexer, body: *Ir.Body) !void {
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
                _ = try self.ir.createVar(self.allocator, body, var_name, .{ .primitive = .number });
            }
        } else if (kind == TokenKind.Char) {
            try fetchExpectMany(lexer, .{.EndOfLine});
            for (vars.items) |var_name| {
                _ = try self.ir.createVar(self.allocator, body, var_name, .{ .primitive = .char });
            }
        } else if (kind == TokenKind.Array) {
            try fetchExpectMany(lexer, .{.OpenSquare});
            const token_array_size = try getExpect(lexer, .IntegerLiteral);
            const array_size = try token_array_size.asUsize();
            try fetchExpectMany(lexer, .{ .CloseSquare, .Of, .Numeric, .EndOfLine });

            for (vars.items) |var_name| {
                _ = try self.ir.createVar(self.allocator, body, var_name, .{ .array = .{ .type = .number, .len = array_size } });
            }
        } else {
            try diagnostic(lexer, token, "Expected a type for var but none was found\n", .{});
            return CompileError.UnexpectedToken;
        }
    }
}

fn compileFunction(self: *Self, lexer: *Lexer) !void {
    try fetchExpectMany(lexer, .{.Function});
    const functionName = try getExpect(lexer, .Identifier);
    std.debug.print("Function: {f}\n", .{functionName});
    try fetchExpectMany(lexer, .{.OpenParent});
    // TODO: Parse function arguments
    try fetchExpectMany(lexer, .{ .CloseParent, .Colon });
    // TODO: Parse function return type
    const function_return_type = lexer.next().?;
    std.debug.print("Return Type: {f}\n", .{function_return_type});
    try fetchExpectMany(lexer, .{.EndOfLine});
    try fetchExpectMany(lexer, .{ .Begin, .EndOfLine });

    var function = Ir.Function{
        .name = try functionName.asString(self.allocator),
        .body = .{
            .variables = .empty,
            .operations = .empty,
        },
    };
    try self.parseBody(lexer, &function.body, .{.End});
    try self.ir.functions.append(self.allocator, function);

    try fetchExpectMany(lexer, .{ .End, .EndOfLine });
}

fn compileProcedure(self: *Self, lexer: *Lexer) !void {
    _ = self;
    try fetchExpectMany(lexer, .{.Procedure});
    const procedureName = try getExpect(lexer, .Identifier);
    std.debug.print("Procedure: {f}\n", .{procedureName});
    try fetchExpectMany(lexer, .{.OpenParent});

    // TODO: Parse procedure arguments
    try fetchExpectMany(lexer, .{ .Identifier, .Colon, .Numeric, .CloseParent, .EndOfLine });

    try fetchExpectMany(lexer, .{ .Begin, .EndOfLine });
    //TODO: Parse function body
    try fetchExpectMany(lexer, .{ .End, .EndOfLine });
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

pub fn compileExpression(self: *Self, lexer: *Lexer, body: *Ir.Body) CompileError!Ir.Arg {
    const lhs = try self.parsePrimary(lexer, body);
    return try self.compileExpressionRecursive(lexer, body, lhs, 0);
}

pub fn parsePrimary(self: *Self, lexer: *Lexer, body: *Ir.Body) CompileError!Ir.Arg {
    const token = lexer.next().?;
    const arg: Ir.Arg = switch (token.kind) {
        // Var or Function call
        .Identifier => arg: {
            const peek = if (lexer.peek()) |peek| peek.kind else TokenKind.Unknown;

            if (peek == TokenKind.OpenParent) {
                try fetchExpectMany(lexer, .{.OpenParent});
                const ArgList = std.ArrayList(Ir.Arg);
                var args: ArgList = .empty;

                while (lexer.peek().?.kind != TokenKind.CloseParent) {
                    const arg = try self.compileExpression(lexer, body);
                    try args.append(self.allocator, arg);

                    if (lexer.peek().?.kind == TokenKind.Comma) {
                        _ = lexer.next();
                    }
                }
                try fetchExpectMany(lexer, .{.CloseParent});

                // The result of the function is stored in a temporary variable
                const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .number });
                try body.operations.append(self.allocator, .{ .call = .{
                    .name = token.token,
                    .offset = address,
                    .args = try args.toOwnedSlice(self.allocator),
                } });

                break :arg Ir.Arg{ .variable = address };
            } else if (peek == TokenKind.OpenSquare) {
                try fetchExpectMany(lexer, .{.OpenSquare});
                const arg = try self.compileExpression(lexer, body);
                try fetchExpectMany(lexer, .{.CloseSquare});

                const variable = self.ir.findVariable(body, token.token) orelse {
                    try diagnostic(lexer, token, "Variable '{s}' is not defined.\n", .{token.token});
                    return error.UnexpectedToken;
                };

                const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .number });
                try body.operations.append(self.allocator, .{ .index = .{
                    .offset = address,
                    .var_address = variable.offset,
                    .var_index = arg,
                } });

                break :arg Ir.Arg{ .deref = address };
            } else {
                const variable = self.ir.findVariable(body, token.token) orelse {
                    try diagnostic(lexer, token, "Variable '{s}' is not defined.\n", .{token.token});
                    return error.UnexpectedToken;
                };
                break :arg Ir.Arg{ .variable = variable.offset };
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
            const variable = Ir.GlobalVar{
                .name = "data",
                .data = try token.asString(self.allocator),
                .address = dataOffset,
            };
            try self.ir.globals.append(self.allocator, variable);
            break :arg Ir.Arg{ .dataLiteral = dataOffset };
        },
        .OpenParent => arg: {
            const lhs = try self.compileExpression(lexer, body);
            try fetchExpectMany(lexer, .{.CloseParent});
            break :arg lhs;
        },
        // Deref
        .Ampersand => arg: {
            const arg = try self.compileExpression(lexer, body);
            const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .pointer });
            try body.operations.append(self.allocator, .{
                .ref = .{ .offset = address, .arg = arg },
            });
            break :arg Ir.Arg{ .variable = address };
        },
        .Multiply => arg: {
            const arg = try self.parsePrimary(lexer, body);
            break :arg Ir.Arg{ .deref = arg.variable };
        },
        // Unary Not
        .Exclamation => arg: {
            const arg = try self.compileExpression(lexer, body);
            const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .boolean });
            try body.operations.append(self.allocator, .{
                .unary_not = .{ .offset = address, .arg = arg },
            });
            break :arg Ir.Arg{ .variable = address };
        },
        .Minus => arg: {
            const arg = try self.parsePrimary(lexer, body);
            const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .number });
            try body.operations.append(self.allocator, .{
                .unary_neg = .{ .offset = address, .arg = arg },
            });
            break :arg Ir.Arg{ .variable = address };
        },
        else => {
            try diagnostic(lexer, token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };
    return arg;
}

pub fn compileExpressionRecursive(self: *Self, lexer: *Lexer, body: *Ir.Body, arg: Ir.Arg, precedence: usize) CompileError!Ir.Arg {
    var lookahead = lexer.peek() orelse return error.UnexpectedEOF;
    var lhs = arg;
    while (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) >= precedence) {
        _ = lexer.next() orelse return error.UnexpectedEOF;
        const kind: TokenKind = lookahead.kind;
        var rhs = try self.parsePrimary(lexer, body);

        lookahead = lexer.peek() orelse return error.UnexpectedEOF;
        if (isOperator(lookahead.kind) and getPrecedence(lookahead.kind) > getPrecedence(kind)) {
            rhs = try self.compileExpressionRecursive(lexer, body, rhs, getPrecedence(kind) + 1);
            lookahead = lexer.peek() orelse return error.UnexpectedEOF;
        }

        // TODO if kind is assigment this temporary varialbe is not necessary
        const address = try self.ir.createTempVar(self.allocator, body, .{ .primitive = .number });

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
        try body.operations.append(self.allocator, op);

        // TODO not every operation return an address, see asigment
        lhs = Ir.Arg{ .variable = address };
    }
    return lhs;
}

pub fn parseBody(self: *Self, lexer: *Lexer, body: *Ir.Body, close_body_tags: anytype) CompileError!void {
    while (lexer.peek()) |peek| {
        if (peek.kind == TokenKind.Return) {
            try fetchExpectMany(lexer, .{.Return});
            const arg = try self.compileExpression(lexer, body);
            try body.operations.append(self.allocator, .{
                .ret = .{ .arg = arg },
            });
            try fetchExpectMany(lexer, .{.EndOfLine});
        } else if (peek.kind == TokenKind.If) {
            try fetchExpectMany(lexer, .{.If});

            // if boolean expression
            var arg = try self.compileExpression(lexer, body);
            try fetchExpectMany(lexer, .{ .Then, .EndOfLine });

            const end_if = try self.ir.createLabel(self.allocator, "END_IF");
            var jump_forward = try self.ir.createLabel(self.allocator, "IF_COND_FALSE");

            try body.operations.append(self.allocator, Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
            try self.parseBody(lexer, body, .{ .Else, .ElseIf, .EndIf });
            try body.operations.append(self.allocator, Ir.Op{ .jump = end_if });
            var nextToken = lexer.peek() orelse return error.UnexpectedEOF;

            while (nextToken.kind == .ElseIf) {
                try fetchExpectMany(lexer, .{.ElseIf});
                try body.operations.append(self.allocator, .{ .label = jump_forward });
                jump_forward = try self.ir.createLabel(self.allocator, "ELSE_IF_COND_FALSE");
                arg = try self.compileExpression(lexer, body);

                // Consume Then
                try fetchExpectMany(lexer, .{ .Then, .EndOfLine });
                try body.operations.append(self.allocator, Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
                try self.parseBody(lexer, body, .{ .ElseIf, .Else, .EndIf });
                try body.operations.append(self.allocator, Ir.Op{ .jump = end_if });

                nextToken = lexer.peek() orelse return error.UnexpectedEOF;
            }

            if (nextToken.kind == .Else) {
                try fetchExpectMany(lexer, .{ .Else, .EndOfLine });
                try body.operations.append(self.allocator, .{ .label = jump_forward });
                try self.parseBody(lexer, body, .{.EndIf});
            } else {
                try body.operations.append(self.allocator, .{ .label = jump_forward });
            }
            try fetchExpectMany(lexer, .{ .EndIf, .EndOfLine });
            try body.operations.append(self.allocator, .{ .label = end_if });
        } else if (peek.kind == TokenKind.While) {
            try fetchExpectMany(lexer, .{.While});

            // Create label before while expression
            const while_loop = try self.ir.createLabel(self.allocator, "WHILE");
            try body.operations.append(self.allocator, .{ .label = while_loop });

            // while boolean expression
            const arg = try self.compileExpression(lexer, body);

            // Jump to label after while
            const while_end = try self.ir.createLabel(self.allocator, "END_WHILE");
            const jump_if = Ir.Op{ .jump_if_false = .{ .label = while_end, .arg = arg } };
            try body.operations.append(self.allocator, jump_if);

            try fetchExpectMany(lexer, .{ .Do, .EndOfLine });
            try self.parseBody(lexer, body, .{.EndWhile});
            try fetchExpectMany(lexer, .{ .EndWhile, .EndOfLine });

            // Jump to start of while
            try body.operations.append(self.allocator, .{ .jump = while_loop });
            // Create label after while
            try body.operations.append(self.allocator, .{ .label = while_end });
        } else if (peek.kind == TokenKind.Identifier or peek.kind == TokenKind.Multiply) {
            _ = try self.compileExpression(lexer, body);
            try fetchExpectMany(lexer, .{.EndOfLine});
        } else if (peek.kind == TokenKind.EndOfLine) {
            try fetchExpectMany(lexer, .{.EndOfLine});
            continue;
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

pub fn accept(lexer: *Lexer, token: ?Token, kind: TokenKind) CompileError!bool {
    const t = token orelse {
        diagnostic(lexer, null, "Expected {s}, got instead 'end of file' \n", .{
            @tagName(kind),
        }) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    };
    return t.kind == kind;
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

/// Peek the next token and check for the expected kind in the list
pub fn peekAny(lexer: *Lexer, comptime kinds: anytype) !bool {
    const fields = std.meta.fields(@TypeOf(kinds));
    const peek = lexer.peek();
    inline for (fields) |field| {
        if (try accept(lexer, peek, @field(kinds, field.name))) {
            return true;
        }
    }
    return false;
}

pub fn diagnostic(lexer: *Lexer, token: ?Token, comptime fmt: []const u8, args: anytype) !void {
    const t = token orelse {
        System.Err.print("Unexpected 'end of file'\nMake sure to balance every 'BEGIN' with and 'END'\n", .{});
        return error.UnexpectedToken;
    };
    const loc = lexer.getLoc(t);
    System.Err.print("{s}:{d}:{d} : " ++ "\x1b[31m" ++ "L3P Compilation Error " ++ "\x1b[0m", .{ lexer.file_name, loc.row, loc.col });
    System.Err.print(fmt, args);
}
