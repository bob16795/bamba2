const std = @import("std");

pub const scannerError = error{
    InvalidChar,
    OutOfMemory,
};

pub const Scanner = struct {
    const Self = @This();
    src: []const u8,
    start: [*]const u8,
    current: [*]const u8,
    line: usize = 1,
    col: usize = 0,

    pub fn init(file: []const u8, src: []const u8) Self {
        std.debug.print("BAM {s}\n", .{file});

        return Scanner{
            .src = src,
            .start = src.ptr,
            .current = src.ptr,
        };
    }

    pub fn nextToken(self: *Self) scannerError!Token {
        self.skipWhitespace();
        self.start = self.current;

        if (self.isAtEnd()) return self.makeToken(.EOF);

        const c = self.advance();
        if (std.ascii.isAlphabetic(c)) return self.identifier();
        if (c == '"') return self.string();
        if (c == '\'') return self.char();

        return switch (c) {
            '&' => self.makeToken(.AMPERSAND),
            '|' => self.makeToken(.BAR),
            '+' => self.makeToken(.PLUS),
            '.' => self.makeToken(.DOT),
            '$' => self.makeToken(.DOLLAR),
            '{' => self.makeToken(.LEFT_BRACE),
            '}' => self.makeToken(.RIGHT_BRACE),
            '(' => self.makeToken(.LEFT_PAREN),
            ')' => self.makeToken(.RIGHT_PAREN),
            '[' => self.makeToken(.LEFT_BRACKET),
            ']' => self.makeToken(.RIGHT_BRACKET),
            ';' => self.makeToken(.SEMI_COLON),
            ':' => self.makeToken(.COLON),
            '/' => self.makeToken(.SLASH),
            '~' => self.makeToken(.TILDE),
            '%' => self.makeToken(.PERCENT),
            ',' => self.makeToken(.COMMA),
            '*' => self.makeToken(.STAR),
            '@' => self.makeToken(.AT),
            '<' => self.makeToken(.LT),
            '>' => self.makeToken(.GT),
            '!' => return if (self.match('=')) self.makeToken(.EXCLAIM_EQUAL) else self.makeToken(.EXCLAIM),
            '=' => return if (self.match('=')) self.makeToken(.EQUAL_EQUAL) else self.makeToken(.EQUAL),
            '-' => return if (self.match('>')) self.makeToken(.ARROW) else self.makeToken(.MINUS),
            '0'...'9' => self.number(),
            else => blk: {
                std.log.info("{c}", .{c});
                break :blk error.InvalidChar;
            },
        };
    }

    pub fn next(self: *Self) !?Token {
        var result = try self.nextToken();
        if (result.kind == .EOF) return null;
        return result;
    }

    inline fn advance(self: *Self) u8 {
        defer self.current += 1;
        return self.current[0];
    }

    inline fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.current[0] != expected) return false;
        self.current += 1;
        return true;
    }

    fn isAtEnd(self: *Self) bool {
        const curr_len = @ptrToInt(self.current) - @ptrToInt(self.src.ptr);
        return curr_len >= self.src.len;
    }

    inline fn peek(self: *Self) u8 {
        return self.current[0];
    }

    inline fn peekNext(self: *Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.current[1];
    }

    fn skipWhitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn number(self: *Self) Token {
        while (std.ascii.isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();
            while (std.ascii.isDigit(self.peek())) _ = self.advance();
        }
        return self.makeToken(if (std.mem.containsAtLeast(u8, self.currentLexeme(), 1, ".")) .FLOAT else .NUMBER);
    }

    fn identifier(self: *Self) Token {
        while (self.peek() == '_' or std.ascii.isDigit(self.peek()) or std.ascii.isAlphabetic(self.peek())) _ = self.advance();
        return self.makeToken(self.identifierType());
    }

    fn char(self: *Self) Token {
        while (self.peek() != '\'') _ = self.advance();
        self.start += 1;
        var result = self.makeToken(.CHAR);
        _ = self.advance();
        return result;
    }

    fn string(self: *Self) Token {
        while (self.peek() != '"') _ = self.advance();
        self.start += 1;
        var result = self.makeToken(.STRING);
        _ = self.advance();
        return result;
    }

    fn identifierType(self: *Self) TokenType {
        const eql = std.mem.eql;
        const lexeme = self.currentLexeme();
        if (eql(u8, lexeme, "interface")) return .INTERFACE;
        if (eql(u8, lexeme, "return")) return .RET;
        if (eql(u8, lexeme, "extfn")) return .EXTERN;
        if (eql(u8, lexeme, "class")) return .STRUCT;
        if (eql(u8, lexeme, "inline")) return .INLINE;
        if (eql(u8, lexeme, "import")) return .IMPORT;
        if (eql(u8, lexeme, "embed")) return .EMBED;
        if (eql(u8, lexeme, "const")) return .CONST;
        if (eql(u8, lexeme, "while")) return .WHILE;
        if (eql(u8, lexeme, "fn")) return .PROC;
        if (eql(u8, lexeme, "prop")) return .PROP;
        if (eql(u8, lexeme, "else")) return .ELSE;
        if (eql(u8, lexeme, "def")) return .DEF;
        if (eql(u8, lexeme, "new")) return .VAR;
        if (eql(u8, lexeme, "if")) return .IF;

        return .IDENTIFIER;
    }

    fn currentLexeme(self: *Self) []const u8 {
        const end = @ptrToInt(self.current) - @ptrToInt(self.start);
        return self.start[0..end];
    }

    fn makeToken(self: *Self, ty: TokenType) Token {
        return Token{
            .line = self.line,
            .col = self.col,
            .kind = ty,
            .lexeme = self.currentLexeme(),
        };
    }

    fn errorToken(self: *Self, comptime msg: []const u8) Token {
        return Token{
            .line = self.line,
            .kind = .TOKEN_ERROR,
            .lexeme = msg,
        };
    }

    pub const Token = struct {
        lexeme: []const u8,
        line: usize,
        col: usize,
        kind: TokenType,

        pub fn format(
            self: Token,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            _ = fmt;

            try writer.print("{s}: {s} @ {}:{}", .{ @tagName(self.kind), self.lexeme, self.line, self.col });
        }
    };

    pub const TokenType = enum {
        // single char tokens
        LEFT_BRACE,
        RIGHT_BRACE,
        LEFT_BRACKET,
        RIGHT_BRACKET,
        LEFT_PAREN,
        RIGHT_PAREN,
        SEMI_COLON,
        AMPERSAND,
        PERCENT,
        DOLLAR,
        TILDE,
        COLON,
        COMMA,
        SLASH,
        STAR,
        PLUS,
        DOT,
        BAR,
        AT,
        LT,
        GT,

        // one or two char tokens
        BANG,
        BANG_EQUAL,
        EQUAL,
        EQUAL_EQUAL,
        EXCLAIM,
        EXCLAIM_EQUAL,
        MINUS,
        ARROW,

        // keywords
        INTERFACE,
        EXTERN,
        INLINE,
        STRUCT,
        IMPORT,
        EMBED,
        CONST,
        WHILE,
        PROC,
        PROP,
        ELSE,
        DEF,
        VAR,
        RET,
        IF,

        // multi char
        IDENTIFIER,
        STRING,
        NUMBER,
        FLOAT,
        CHAR,

        // Control
        EOF,
        TOKEN_ERROR,
    };
};
