const std = @import("std");
const upperString = std.ascii.upperString;
const eql = std.mem.eql;
const isHex = std.ascii.isHex;
const isDigit = std.ascii.isDigit;

pub const keywords = [_][]const u8{
    "PROGRAM",
    "FUNCTION",
    "PROCEDURE",
    "RETURN",
    "VAR",
    "NUMERIC",
    "CHAR",
    "REAL",
    "BEGIN",
    "WHILE",
    "DO",
    "ENDWHILE",
    "IF",
    "THEN",
    "ELSE",
    "ELSEIF",
    "ENDIF",
    "END",
    "AND",
    "OR",
    "NOT",
    "ARRAY",
    "OF",
};

pub const keywords_map = [_]TokenKind{
    .Program,
    .Function,
    .Procedure,
    .Return,
    .Var,
    .Numeric,
    .Char,
    .Real,
    .Begin,
    .While,
    .Do,
    .EndWhile,
    .If,
    .Then,
    .Else,
    .ElseIf,
    .EndIf,
    .End,
    .And,
    .Or,
    .Not,
    .Array,
    .Of,
};

pub const TokenKind = enum {
    Identifier,
    // keywords
    Program,
    Function,
    Procedure,
    Return,
    Var,
    Numeric,
    Char,
    Real,
    Array,
    Of,
    Begin,
    While,
    Do,
    EndWhile,
    End,
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
    // Literals
    IntegerLiteral,
    StringLiteral,
    // Symbols
    OpenParent,
    CloseParent,
    OpenSquare,
    CloseSquare,
    Comment,
    Comma,
    Equal,
    Exclamation,
    Plus,
    PlusEqual,
    Minus,
    MinusEqual,
    Multiply,
    MultiplyEqual,
    Divide,
    DivideEqual,
    Hat,
    Ampersand,
    Pipe,
    Colon,
    Assign,
    ExclamationEqual,
    SemiColon,
    Less,
    LessLess,
    LessEqual,
    Greater,
    GreaterGreater,
    GreaterEqual,
    Percent,
    Not,
    And,
    Or,
    EndOfLine,
    Unknown,

    pub fn getKeyword(needle: []const u8) ?TokenKind {
        if (keywords.len != keywords_map.len) {
            @compileError("Keywords and keywords_map must have the same length");
        }
        for (keywords, 0..) |keyword, i| {
            var buffer: [1024]u8 = undefined;
            if (buffer.len >= needle.len) {
                const upNeedle = upperString(&buffer, needle);
                if (eql(u8, upNeedle, keyword)) {
                    return keywords_map[i];
                }
            }
        }
        return null;
    }
};

pub const Loc = struct {
    row: usize,
    col: usize,
};

pub const Token = struct {
    token: []const u8,
    kind: TokenKind,
    start: usize,

    pub fn format(self: Token, w: *std.Io.Writer) !void {
        if (self.kind == TokenKind.EndOfLine) {
            try w.print("{}", .{self.kind});
        } else {
            try w.print("{} \"{s}\"", .{ self.kind, self.token });
        }
    }

    pub fn asString(self: Token, allocator: std.mem.Allocator) ![]const u8 {
        if (self.token[0] == '\'' and self.token[self.token.len - 1] == '\'') {
            const no_sigle_quote = self.token[1 .. self.token.len - 1];
            const double_quoted = try std.fmt.allocPrint(allocator, "\"{s}\"", .{no_sigle_quote});
            defer allocator.free(double_quoted);
            return try std.zig.string_literal.parseAlloc(allocator, double_quoted);
        } else {
            return try allocator.dupe(u8, self.token);
        }
    }

    pub fn asInteger(self: Token) !u32 {
        return try std.fmt.parseInt(u32, self.token, 0);
    }

    pub fn asUsize(self: Token) !usize {
        const int = try self.asInteger();
        return @as(usize, @intCast(int));
    }

    pub fn asFloat(self: Token) !f32 {
        return try std.fmt.parseFloat(type, self.token);
    }
};

file_name: []const u8,
buffer: []const u8,
token_start: usize,
token_end: usize,

pub fn init(file_name: []const u8, buffer: []const u8) @This() {
    return @This(){
        .file_name = file_name,
        .buffer = buffer,
        .token_start = 0,
        .token_end = 0,
    };
}

pub fn getLoc(self: *@This(), token: Token) Loc {
    var index: usize = 0;
    const eof = self.buffer.len;

    var row: usize = 1;
    var col: usize = 0;

    while (index < eof and index <= token.start) : (index += 1) {
        switch (self.buffer[index]) {
            '\n' => {
                row += 1;
                col = 0;
            },
            else => {
                col += 1;
            },
        }
    }
    return .{ .row = row, .col = col };
}

fn isDigitBase(char: u8) bool {
    return char == 'b' or char == 'o' or char == 'x';
}

fn isIdentifier(char: u8) bool {
    return char == '_' or std.ascii.isAlphanumeric(char);
}

pub fn peek(self: *@This()) ?Token {
    const savepoint = .{ self.token_start, self.token_end };
    const next_token = self.get();
    self.token_start = savepoint[0];
    self.token_end = savepoint[1];
    return next_token;
}

pub fn next(self: *@This()) ?Token {
    const token = self.get();
    return token;
}

fn get(self: *@This()) ?Token {
    const eof = self.buffer.len;
    var kind = TokenKind.Unknown;
    while (self.token_start < eof) {
        const cursor = self.buffer[self.token_start];
        switch (cursor) {
            ' ' => {
                self.token_start += 1;
                self.token_end = self.token_start;
                continue;
            },
            '\n' => {
                self.token_end = self.token_start;
                while (self.token_end < eof and (self.buffer[self.token_end]) == '\n') {
                    self.token_end += 1;
                }

                if (kind != TokenKind.Comment) {
                    kind = TokenKind.EndOfLine;
                }
            },
            '{' => {
                self.token_end = self.token_start + 1;
                while (self.token_end < eof and self.buffer[self.token_end] != '}') {
                    self.token_end += 1;
                }
                if (self.token_end < eof and self.buffer[self.token_end] == '}') {
                    self.token_end += 1;
                }
                kind = TokenKind.Comment;
            },
            '\'' => {
                self.token_end = self.token_start + 1;
                while (self.token_end < eof and self.buffer[self.token_end] != '\'') {
                    self.token_end += 1;
                }

                if (self.token_end < eof and self.buffer[self.token_end] == '\'') {
                    self.token_end += 1;
                }

                kind = TokenKind.StringLiteral;
            },
            '+' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Plus;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.PlusEqual;
                    self.token_end += 1;
                }
            },
            '-' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Minus;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.MinusEqual;
                    self.token_end += 1;
                }
            },
            '<' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Less;
                if (self.token_end < eof and self.buffer[self.token_end] == '<') {
                    kind = TokenKind.LessLess;
                    self.token_end += 1;
                } else if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.LessEqual;
                    self.token_end += 1;
                }
            },
            '>' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Greater;
                if (self.token_end < eof and self.buffer[self.token_end] == '>') {
                    kind = TokenKind.GreaterGreater;
                    self.token_end += 1;
                } else if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.GreaterEqual;
                    self.token_end += 1;
                }
            },
            '*' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Multiply;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.MultiplyEqual;
                    self.token_end += 1;
                }
            },
            '/' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Divide;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.DivideEqual;
                    self.token_end += 1;
                }
            },
            ':' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Colon;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.Assign;
                    self.token_end += 1;
                }
            },
            '!' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Exclamation;
                if (self.token_end < eof and self.buffer[self.token_end] == '=') {
                    kind = TokenKind.ExclamationEqual;
                    self.token_end += 1;
                }
            },
            '%' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Percent;
            },
            '=' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Equal;
            },
            ',' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Comma;
            },
            '(' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.OpenParent;
            },
            ')' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.CloseParent;
            },
            '[' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.OpenSquare;
            },
            ']' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.CloseSquare;
            },
            ';' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.SemiColon;
            },
            '^' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Hat;
            },
            '&' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Ampersand;
            },
            '|' => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Pipe;
            },
            '_', 'a'...'z', 'A'...'Z' => {
                self.token_end = self.token_start + 1;
                while (self.token_end < eof and isIdentifier(self.buffer[self.token_end])) {
                    self.token_end += 1;
                }
                const id = self.buffer[self.token_start..self.token_end];
                kind = TokenKind.getKeyword(id) orelse TokenKind.Identifier;
            },
            '0'...'9' => {
                self.token_end = self.token_start + 1;
                if (isDigitBase(self.buffer[self.token_end])) {
                    self.token_end += 1;
                    while (self.token_end < eof and isHex(self.buffer[self.token_end])) {
                        self.token_end += 1;
                    }
                } else {
                    while (self.token_end < eof and isDigit(self.buffer[self.token_end])) {
                        self.token_end += 1;
                    }
                }
                kind = TokenKind.IntegerLiteral;
            },
            else => {
                self.token_end = self.token_start + 1;
                kind = TokenKind.Unknown;
            },
        }

        if (kind == TokenKind.Comment) {
            self.token_start = self.token_end;
            continue;
        }

        const token = self.buffer[self.token_start..self.token_end];
        // std.debug.print("token: ({s})\n", .{@tagName(kind)});
        // std.debug.print("token: ({s})\n", .{name});
        const plex: Token = .{
            .token = token,
            .kind = kind,
            .start = self.token_start,
        };
        self.token_start = self.token_end;

        return plex;
    }
    return null;
}
