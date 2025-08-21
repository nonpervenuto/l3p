const std = @import("std");
const Lexer = @import("Lexer.zig");
const Ir = @import("Ir.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

pub const CompileError = error{
    OutOfMemory,
    Overflow,
    DivisionByZero,
    InvalidCharacter,
    InvalidLiteral,
    UnexpectedToken,
    UnexpectedEOF,
    PrintDiagnosticError,
};

allocator: std.mem.Allocator,
std_err: std.fs.File.DeprecatedWriter,
ir: Ir,

pub fn init(allocator: std.mem.Allocator) @This() {
    const std_err = std.fs.File.stderr().deprecatedWriter();
    return @This(){
        .allocator = allocator,
        .std_err = std_err,
        .ir = .{
            .variables = Ir.VariableList.init(allocator),
            .operations = Ir.OperationList.init(allocator),
        },
    };
}

pub fn compile(self: *@This(), path: []const u8, buffer: []const u8) !Ir {
    var lexer = Lexer.init(path, buffer);
    try self.getAndExpect(&lexer, .{ .Program, .Identifier, .EndOfLine });
    while (lexer.next()) |current| {
        if (current.kind == TokenKind.Var) {
            const var_name_token = lexer.next();
            try self.expect(var_name_token, .Identifier);
            const var_name = var_name_token.?.token;

            // TODO: parse data types
            try self.getAndExpect(&lexer, .{ .Colon, .Numeric, .EndOfLine });

            if (self.ir.findVariable(var_name)) |_| {
                try self.printError(var_name_token, "Variable '{s}' already declared\n", .{
                    var_name,
                });
                return error.UnexpectedToken;
            }

            const offset = self.ir.calcVarOffset(Ir.DataType.numeric);
            const variable = Ir.Declaration{ .var_dec = .{
                .name = var_name,
                .offset = offset,
                .type = Ir.DataType.numeric,
            } };

            self.ir.variables.append(variable) catch unreachable;
            continue;
            //
        } else if (current.kind == TokenKind.Begin) {
            try self.getAndExpect(&lexer, .{.EndOfLine});
            // parse body until end
            try self.parseBody(&lexer, .{.End});
            try self.getAndExpect(&lexer, .{ .End, .EndOfLine });
        }
    }
    return self.ir;
}

fn isOperator(k: TokenKind) bool {
    return switch (k) {
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
        else => 0,
    };
}

pub fn compileExpression(self: *@This(), lexer: *Lexer) CompileError!Ir.Arg {
    const lhs = try self.parsePrimary(lexer);
    return try self.compileExpressionRecursive(lexer, lhs, 0);
}

pub fn parsePrimary(self: *@This(), lexer: *Lexer) CompileError!Ir.Arg {
    const token = lexer.next().?;
    const arg: Ir.Arg = switch (token.kind) {
        .Identifier => arg: {
            const declaration = self.ir.findVariable(token.token) orelse {
                try self.printError(token, "Variable '{s}' not defined \n", .{token.token});
                return error.UnexpectedToken;
            };
            break :arg Ir.Arg{ .variable = declaration.var_dec.offset };
        },
        .IntegerLiteral => arg: {
            const value = try token.asInteger();
            break :arg Ir.Arg{ .integerLiteral = value };
        },
        .StringLiteral => arg: {
            const dataOffset = self.ir.calcGlobalOffset();
            const variable = Ir.Declaration{ .global_dec = .{
                .data = try token.asString(self.allocator),
                .address = dataOffset,
            } };
            try self.ir.variables.append(variable);
            break :arg Ir.Arg{ .dataLiteral = dataOffset };
        },
        .OpenParent => arg: {
            const lhs = try self.compileExpression(lexer);
            try self.getAndExpect(lexer, .{.CloseParent});
            break :arg lhs;
        },
        .Not => {
            @panic("Op Not not implemented");
        },
        .Minus => arg: {
            const arg = try self.parsePrimary(lexer);
            switch (arg) {
                .integerLiteral => |value| {
                    break :arg Ir.Arg{ .integerLiteral = -value };
                },
                else => {
                    const address = try self.ir.createTempVar(Ir.DataType.numeric);
                    try self.ir.operations.append(.{
                        .prefix_neg = .{ .offset = address, .arg = arg },
                    });
                    break :arg Ir.Arg{ .variable = address };
                },
            }
        },
        else => {
            try self.printError(token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };
    return arg;
}

pub fn compileExpressionRecursive(self: *@This(), lexer: *Lexer, arg: Ir.Arg, precedence: usize) CompileError!Ir.Arg {
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

        const address = try self.ir.createTempVar(Ir.DataType.numeric);

        const op: Ir.Op = switch (kind) {
            .Or => .{ .infix_or = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .And => .{ .infix_and = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Pipe => @panic("| non implemented"),
            .Hat => @panic("^ non implemented"),
            .Ampersand => @panic("& non implemented"),
            .Equal => .{ .infix_eq = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .ExclamationEqual => @panic("!= non implemented"),
            .Less => .{ .infix_l = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .LessEqual => .{ .infix_le = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .GreaterEqual => .{ .infix_ge = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Greater => .{ .infix_g = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .LessLess => @panic("<< not implemented"),
            .GreaterGreater => @panic(">> not implemented"),
            .Plus => .{ .infix_plus = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Minus => .{ .infix_minus = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Multiply => .{ .infix_multiply = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            .Divide => blk: {
                if (rhs.integerLiteral == 0) {
                    try self.printError(lookahead, "Division by zero detected!\n", .{});
                    return error.DivisionByZero;
                }
                break :blk .{ .infix_divide = .{ .offset = address, .lhs = lhs, .rhs = rhs } };
            },
            .Percent => .{ .infix_modulo = .{ .offset = address, .lhs = lhs, .rhs = rhs } },
            else => @panic("Not implemented"),
        };
        try self.ir.operations.append(op);

        lhs = Ir.Arg{ .variable = address };
    }
    return lhs;
}

pub fn parseBody(self: *@This(), lexer: *Lexer, close_body_tags: anytype) CompileError!void {
    while (lexer.peek()) |peek| {
        if (peek.kind == TokenKind.If) {
            try self.getAndExpect(lexer, .{.If});

            // if boolean expression
            var arg = try self.compileExpression(lexer);
            try self.getAndExpect(lexer, .{ .Then, .EndOfLine });

            const end_if = try self.ir.createLabel(self.allocator, "END_IF");
            var jump_forward = try self.ir.createLabel(self.allocator, "IF_COND_FALSE");

            try self.ir.operations.append(Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
            try self.parseBody(lexer, .{ .Else, .ElseIf, .EndIf });
            try self.ir.operations.append(Ir.Op{ .jump = end_if });
            var nextToken = lexer.peek() orelse return error.UnexpectedEOF;

            while (nextToken.kind == .ElseIf) {
                try self.getAndExpect(lexer, .{.ElseIf});
                try self.ir.operations.append(.{ .label = jump_forward });
                jump_forward = try self.ir.createLabel(self.allocator, "ELSE_IF_COND_FALSE");
                arg = try self.compileExpression(lexer);

                // Consume Then
                try self.getAndExpect(lexer, .{ .Then, .EndOfLine });
                try self.ir.operations.append(Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
                try self.parseBody(lexer, .{ .ElseIf, .Else, .EndIf });
                try self.ir.operations.append(Ir.Op{ .jump = end_if });

                nextToken = lexer.peek() orelse return error.UnexpectedEOF;
            }

            if (nextToken.kind == .Else) {
                try self.getAndExpect(lexer, .{ .Else, .EndOfLine });
                try self.ir.operations.append(.{ .label = jump_forward });
                try self.parseBody(lexer, .{.EndIf});
            } else {
                try self.ir.operations.append(.{ .label = jump_forward });
            }
            try self.getAndExpect(lexer, .{ .EndIf, .EndOfLine });
            try self.ir.operations.append(.{ .label = end_if });
        } else if (peek.kind == TokenKind.While) {
            try self.getAndExpect(lexer, .{.While});

            // Create label before while expression
            const while_loop = try self.ir.createLabel(self.allocator, "WHILE");
            try self.ir.operations.append(.{ .label = while_loop });

            // while boolean expression
            const arg = try self.compileExpression(lexer);

            // Jump to label after while
            const while_end = try self.ir.createLabel(self.allocator, "END_WHILE");
            const jump_if = Ir.Op{ .jump_if_false = .{ .label = while_end, .arg = arg } };
            try self.ir.operations.append(jump_if);

            try self.getAndExpect(lexer, .{ .Do, .EndOfLine });
            try self.parseBody(lexer, .{.EndWhile});
            try self.getAndExpect(lexer, .{ .EndWhile, .EndOfLine });

            // Jump to start of while
            try self.ir.operations.append(.{ .jump = while_loop });
            // Create label after while
            try self.ir.operations.append(.{ .label = while_end });
        } else if (peek.kind == TokenKind.Identifier) {
            const id = lexer.next().?.token;
            const peek_token = lexer.peek().?;

            // assign
            if (peek_token.kind == TokenKind.ColonEqual) {
                try self.getAndExpect(lexer, .{.ColonEqual});

                const declaration = self.ir.findVariable(id) orelse {
                    try self.printError(peek, "Variable '{s}' not found\n", .{id});
                    return error.UnexpectedToken;
                };

                const arg = try self.compileExpression(lexer);

                try self.getAndExpect(lexer, .{.EndOfLine});
                try self.ir.operations.append(.{
                    .assign = .{
                        .offset = declaration.var_dec.offset,
                        .arg = arg,
                    },
                });
            } else if (peek_token.kind == TokenKind.OpenParent) {
                try self.getAndExpect(lexer, .{.OpenParent});

                var args = std.ArrayList(Ir.Arg).init(self.allocator);

                while (lexer.peek().?.kind != TokenKind.CloseParent) {
                    const arg = try self.compileExpression(lexer);
                    try args.append(arg);

                    if (lexer.peek().?.kind == TokenKind.Comma) {
                        _ = lexer.next();
                    }
                }
                try self.getAndExpect(lexer, .{ .CloseParent, .EndOfLine });

                try self.ir.operations.append(.{ .call = .{
                    .name = id,
                    .args = try args.toOwnedSlice(),
                } });
            }
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
            try self.printError(peek, "Unexpected token: {s}", .{peek.token});
            return;
        }
    }
}

pub fn expect(self: *@This(), token: ?Token, kind: TokenKind) CompileError!void {
    const t = token orelse {
        self.printError(null, "Expected {s}, got instead 'end of file' \n", .{
            @tagName(kind),
        }) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    };
    if (t.kind != kind) {
        self.printError(t, "Expected keyword '{s}', got instead: {s} '{s}'\n", .{
            @tagName(kind),
            @tagName(t.kind),
            t.token,
        }) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    }
}

pub fn getAndExpect(self: *@This(), lexer: *Lexer, comptime kinds: anytype) !void {
    const fields = std.meta.fields(@TypeOf(kinds));
    inline for (fields) |field| {
        try self.expect(lexer.next(), @field(kinds, field.name));
    }
}

pub fn printError(self: *@This(), token: ?Token, comptime fmt: []const u8, args: anytype) !void {
    const t = token orelse {
        self.std_err.print("Unexpected 'end of file'\nMake sure to balance every 'BEGIN' with and 'END'\n", .{}) catch return error.PrintDiagnosticError;
        return error.UnexpectedToken;
    };
    self.std_err.print("{s}:{d}:{d} : " ++ "\x1b[31m" ++ "L3P Compilation Error\n" ++ "\x1b[0m", .{
        t.file_name,
        t.lineNumber,
        t.token_start,
    }) catch return error.PrintDiagnosticError;
    self.std_err.print(fmt, args) catch return error.PrintDiagnosticError;
}
