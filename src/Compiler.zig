const std = @import("std");
const Lexer = @import("Lexer.zig");
const Ir = @import("Ir.zig");
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;

allocator: std.mem.Allocator,
ir: Ir,

pub fn init(allocator: std.mem.Allocator) @This() {
    return @This(){
        .allocator = allocator,
        .ir = .{
            .variables = Ir.VariableList.init(allocator),
            .operations = Ir.OperationList.init(allocator),
        },
    };
}

pub fn compile(self: *@This(), path: []const u8, buffer: []const u8) !Ir {
    var lexer = Lexer.init(path, buffer);

    // Program
    try getAndExpect(&lexer, .{ .Program, .Identifier, .EndOfLine });
    while (lexer.next()) |current| {
        if (current.kind == TokenKind.Var) {
            const var_name_token = lexer.next();
            try expect(var_name_token, .Identifier);
            const var_name = var_name_token.?.token;

            // TODO: parse data types
            try getAndExpect(&lexer, .{ .Colon, .Numeric, .EndOfLine });

            if (self.ir.findVariable(var_name)) |_| {
                try printError(var_name_token, "Variable '{s}' already declared\n", .{
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
            try getAndExpect(&lexer, .{.EndOfLine});
            try self.parseBody(&lexer, .{.End});
            try getAndExpect(&lexer, .{ .End, .EndOfLine });
        }
    }

    return self.ir;

    // try getAndExpect(&lexer, .{.EndOfLine});
}

pub fn parsePrimary(self: *@This(), lexer: *Lexer) !Ir.Arg {
    const token = lexer.next().?;
    const arg: Ir.Arg = switch (token.kind) {
        .Identifier => arg: {
            const declaration = self.ir.findVariable(token.token) orelse {
                try printError(token, "Variable '{s}' not defined \n", .{token.token});
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
        else => {
            try printError(token, "Unexpected token '{s}'\n", .{token.token});
            return error.UnexpectedToken;
        },
    };
    return arg;
}

fn isOperator(k: TokenKind) bool {
    return switch (k) {
        .Plus, .Minus, .Multiply, .Divide, .Less, .LessEqual, .Greater, .GreaterEqual, .Equal, .Percent, .And, .Or => true,
        else => false,
    };
}

fn getPrecedence(k: TokenKind) usize {
    return switch (k) {
        .And => 1,
        .Or => 2,
        .Less, .LessEqual, .Greater, .GreaterEqual, .Equal => 3,
        .Plus, .Minus => 4,
        .Percent => 5,
        .Multiply, .Divide => 6,
        else => 0,
    };
}

pub fn compileExpression(self: *@This(), lexer: *Lexer) !Ir.Arg {
    const lhs = try self.parsePrimary(lexer);
    const res = try self.compileExpressionRecursive(lexer, lhs, 0);
    // std.debug.print("{} \n", .{res});
    return res;
}

pub fn compileExpressionRecursive(self: *@This(), lexer: *Lexer, arg: Ir.Arg, precedence: usize) !Ir.Arg {
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

        const address = self.ir.calcVarOffset(Ir.DataType.numeric);
        const temp_var = Ir.Declaration{
            .var_dec = .{
                .name = "",
                .offset = address,
                .type = Ir.DataType.numeric,
            },
        };

        try self.ir.variables.append(temp_var);
        switch (op) {
            .Plus => try self.ir.operations.append(.{
                .infix_plus = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Minus => try self.ir.operations.append(.{
                .infix_minus = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Multiply => try self.ir.operations.append(.{
                .infix_multiply = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Percent => try self.ir.operations.append(.{
                .infix_modulo = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Divide => {
                if (rhs.integerLiteral == 0) {
                    try printError(lookahead, "Division by zero detected!\n", .{});
                    return error.DivisionByZero;
                }
                try self.ir.operations.append(.{
                    .infix_divide = .{ .offset = address, .lhs = lhs, .rhs = rhs },
                });
            },
            .Less => try self.ir.operations.append(.{
                .infix_l = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .LessEqual => try self.ir.operations.append(.{
                .infix_le = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Equal => try self.ir.operations.append(.{
                .infix_eq = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .GreaterEqual => try self.ir.operations.append(.{
                .infix_ge = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Greater => try self.ir.operations.append(.{
                .infix_g = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .And => try self.ir.operations.append(.{
                .infix_and = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            .Or => try self.ir.operations.append(.{
                .infix_or = .{ .offset = address, .lhs = lhs, .rhs = rhs },
            }),
            else => @panic("Not implemented"),
        }
        lhs = Ir.Arg{ .variable = address };
    }
    return lhs;
}
pub fn parseBody(self: *@This(), lexer: *Lexer, close_body_tags: anytype) CompileError!void {
    while (lexer.peek()) |peek| {
        if (peek.kind == TokenKind.If) {
            try getAndExpect(lexer, .{.If});

            // if boolean expression
            var arg = try self.compileExpression(lexer);
            try getAndExpect(lexer, .{ .Then, .EndOfLine });

            self.ir.jump_labels += 1;
            const jump_out = self.ir.jump_labels;

            self.ir.jump_labels += 1;
            var jump_forward = self.ir.jump_labels;
            try self.ir.operations.append(Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });

            try self.parseBody(lexer, .{ .Else, .ElseIf, .EndIf });
            try self.ir.operations.append(Ir.Op{ .jump = jump_out });
            var nextToken = lexer.peek();

            while (nextToken.?.kind == .ElseIf) {
                try getAndExpect(lexer, .{.ElseIf});

                try self.ir.operations.append(.{ .label = jump_forward });
                self.ir.jump_labels += 1;
                jump_forward = self.ir.jump_labels;

                arg = try self.compileExpression(lexer);

                // Consume Then
                try getAndExpect(lexer, .{ .Then, .EndOfLine });
                try self.ir.operations.append(Ir.Op{ .jump_if_false = .{ .label = jump_forward, .arg = arg } });
                try self.parseBody(lexer, .{ .ElseIf, .Else, .EndIf });
                try self.ir.operations.append(Ir.Op{ .jump = jump_out });

                nextToken = lexer.peek();
            }

            if (nextToken.?.kind == .Else) {
                try getAndExpect(lexer, .{ .Else, .EndOfLine });
                try self.ir.operations.append(.{ .label = jump_forward });
                try self.parseBody(lexer, .{.EndIf});
            }

            try getAndExpect(lexer, .{ .EndIf, .EndOfLine });
            try self.ir.operations.append(.{ .label = jump_out });
        } else if (peek.kind == TokenKind.While) {
            try getAndExpect(lexer, .{.While});

            // Create label before while expression
            self.ir.jump_labels += 1;
            const inner_label = self.ir.jump_labels;
            try self.ir.operations.append(.{ .label = inner_label });

            // while boolean expression
            const arg = try self.compileExpression(lexer);

            // Jump to label after while
            self.ir.jump_labels += 1;
            const after_while_label = self.ir.jump_labels;
            const jump_if = Ir.Op{ .jump_if_false = .{ .label = after_while_label, .arg = arg } };
            try self.ir.operations.append(jump_if);

            try getAndExpect(lexer, .{ .Do, .EndOfLine });
            try self.parseBody(lexer, .{.EndWhile});
            try getAndExpect(lexer, .{ .EndWhile, .EndOfLine });

            // Jump to start of while
            try self.ir.operations.append(.{ .jump = inner_label });
            // Create label after while
            try self.ir.operations.append(.{ .label = after_while_label });
        } else if (peek.kind == TokenKind.Identifier) {
            const id = lexer.next().?.token;
            const peek_token = lexer.peek().?;

            // assign
            if (peek_token.kind == TokenKind.ColonEqual) {
                try getAndExpect(lexer, .{.ColonEqual});

                const declaration = self.ir.findVariable(id) orelse {
                    try printError(peek, "Variable '{s}' not found\n", .{id});
                    return error.UnexpectedToken;
                };

                const arg = try self.compileExpression(lexer);

                try getAndExpect(lexer, .{.EndOfLine});
                try self.ir.operations.append(.{
                    .assign = .{
                        .offset = declaration.var_dec.offset,
                        .arg = arg,
                    },
                });
            } else if (peek_token.kind == TokenKind.OpenParent) {
                try getAndExpect(lexer, .{.OpenParent});

                // TODO: iterate arguments list
                var args = std.ArrayList(Ir.Arg).init(self.allocator);

                while (lexer.peek().?.kind != TokenKind.CloseParent) {
                    const arg = try self.compileExpression(lexer);
                    try args.append(arg);

                    if (lexer.peek().?.kind == TokenKind.Comma) {
                        _ = lexer.next();
                    }
                }
                try getAndExpect(lexer, .{ .CloseParent, .EndOfLine });

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
            try printError(peek, "Unexpected token: {s}", .{peek.token});
            return;
        }
    }
}

pub fn expect(token: ?Token, kind: TokenKind) CompileError!void {
    const t = token orelse {
        print("Expected {s}, got instead 'end of file' \n", .{
            @tagName(kind),
        });
        return error.UnexpectedToken;
    };
    if (t.kind != kind) {
        print("{s}:{d}:{d}: Expected {s}, got instead: {s} '{s}'\n", .{
            t.file_name,
            t.lineNumber,
            t.token_start,
            @tagName(kind),
            @tagName(t.kind),
            t.token,
        });
        return error.UnexpectedToken;
    }
}

pub fn getAndExpect(lexer: *Lexer, comptime kinds: anytype) !void {
    const fields = std.meta.fields(@TypeOf(kinds));
    inline for (fields) |field| {
        try expect(lexer.next(), @field(kinds, field.name));
    }
}

const CompileError = error{
    OutOfMemory,
    Overflow,
    DivisionByZero,
    InvalidCharacter,
    InvalidLiteral,
    UnexpectedToken,
};

pub fn printError(token: ?Token, comptime fmt: []const u8, args: anytype) !void {
    const t = token orelse {
        print("Unexpected 'end of file' \n", .{});
        return error.UnexpectedToken;
    };
    print("{s}:{d}:{d} : " ++ "\x1b[31m" ++ "L3P Compilation Error\n" ++ "\x1b[0m", .{
        t.file_name,
        t.lineNumber,
        t.token_start,
    });
    print(fmt, args);
}

pub fn print(comptime format: []const u8, args: anytype) void {
    const stdout_file = std.fs.File.stdout().deprecatedWriter();
    stdout_file.print(format, args) catch unreachable;
}
