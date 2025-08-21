const std = @import("std");
const upperString = std.ascii.upperString;
const eql = std.mem.eql;

pub const keywords = [_][]const u8{
    "PROGRAM",
    "VAR",
    "NUMERIC",
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
};

pub const keywords_map = [_]TokenKind{
    .Program,
    .Var,
    .Numeric,
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
};

pub const TokenKind = enum {
    Identifier,
    // keywords
    Program,
    Var,
    Numeric,
    Real,
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
    ColonEqual,
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

pub const Token = struct {
    file_name: []const u8,
    token: []const u8,
    kind: TokenKind,
    lineNumber: usize,
    token_start: usize,
    token_end: usize,

    pub fn format(self: Token, w: *std.Io.Writer) !void {
        if (self.kind == TokenKind.EndOfLine) {
            try w.print("{d}:({d}-{d}) {}", .{ self.lineNumber, self.token_start, self.token_end, self.kind });
        } else {
            try w.print("{d}:({d}-{d}) {} \"{s}\"", .{ self.lineNumber, self.token_start, self.token_end, self.kind, self.token });
        }
    }

    pub fn asString(self: Token, allocator: std.mem.Allocator) ![]const u8 {
        if (self.token[0] == '\'' and self.token[self.token.len - 1] == '\'') {
            // Escape string
            const no_sigle_quote = self.token[1 .. self.token.len - 1];
            const double_quoted = try std.fmt.allocPrint(allocator, "\"{s}\"", .{no_sigle_quote});
            defer allocator.free(double_quoted);
            return try std.zig.string_literal.parseAlloc(allocator, double_quoted);
        } else {
            return try allocator.dupe(u8, self.token);
        }
    }

    pub fn asInteger(self: Token) !i32 {
        return try std.fmt.parseInt(i32, self.token, 0);
    }

    pub fn asFloat(self: Token) !f32 {
        return try std.fmt.parseFloat(type, self.token);
    }
};

file_name: []const u8,
buffer: []const u8,
lineNumber: usize,
token_start: usize,
token_end: usize,

pub fn init(file_name: []const u8, buffer: []const u8) @This() {
    return @This(){
        .file_name = file_name,
        .buffer = buffer,
        .lineNumber = 0,
        .token_start = 0,
        .token_end = 0,
    };
}

pub fn peek(self: *@This()) ?Token {
    const savepoint = .{ self.lineNumber, self.token_start, self.token_end };
    const next_token = self.next();
    self.lineNumber = savepoint[0];
    self.token_start = savepoint[1];
    self.token_end = savepoint[2];
    return next_token;
}

pub fn next(self: *@This()) ?Token {
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
                    self.lineNumber += 1;
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
                    kind = TokenKind.ColonEqual;
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
                while (self.token_end < eof and std.ascii.isAlphanumeric(self.buffer[self.token_end])) {
                    self.token_end += 1;
                }
                const id = self.buffer[self.token_start..self.token_end];
                kind = TokenKind.getKeyword(id) orelse TokenKind.Identifier;
            },
            '0'...'9' => {
                self.token_end = self.token_start + 1;
                while (self.token_end < eof and std.ascii.isDigit(self.buffer[self.token_end])) {
                    self.token_end += 1;
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
            .file_name = self.file_name,
            .token = token,
            .kind = kind,
            .lineNumber = self.lineNumber,
            .token_start = self.token_start,
            .token_end = self.token_end,
        };

        self.token_start = self.token_end;
        return plex;
    }
    return null;
}
