const std = @import("std");
const scanner = @import("scanner.zig");

const parserError = error{
    InvalidChar,
    OutOfMemory,
    ExpectedDef,
    ExpectedEof,
    ExpectedColon,
    ExpectedBrace,
    ExpectedParen,
    ExpectedSemicolon,
    ExpectedString,
    BadDollarExpr,
    BadDefKind,
    InvalidCharacter,
    Overflow,
    FileError,
};

pub const Expression = struct {
    const ExpressionKind = enum {
        ConstInt,
        ConstFloat,
        ConstString,
        Ident,
        Create,
        Operation,
        Struct,
        Paren,
        Proc,
    };

    const Operation = enum {
        // binary
        BitAnd,
        BitOr,
        NotEqual,
        Greater,
        Assign,
        Access,
        IndexAccess,
        Equal,
        Less,
        Mul,
        Div,
        Mod,
        Add,
        Sub,

        // unary
        BitNot,
        Deref,
        Neg,

        // unknown
        ConstOpaque,
        ConstArray,
        Call,
    };

    data: union(ExpressionKind) {
        ConstInt: struct {
            value: usize,
        },
        ConstFloat: struct {
            value: f64,
        },
        ConstString: struct {
            value: []const u8,
        },
        Ident: struct {
            name: []const u8,
        },
        Create: struct {
            kind: *Expression,
        },
        Operation: struct {
            op: Operation,
            values: []Expression,
        },
        Proc: struct {
            in: [][]const u8,
            out: *Expression,
            body: ?[]Statement,
            ext: bool = false,
        },
        Struct: struct {
            body: []Definition,
        },
        Paren: struct {
            expr: *Expression,
        },
    },
    line: usize,
    col: usize,

    pub fn format(
        self: Expression,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        switch (self.data) {
            .ConstInt => |data| {
                try writer.print("#{}", .{data.value});
            },
            .ConstFloat => |data| {
                try writer.print("#f{}", .{data.value});
            },
            .ConstString => |data| {
                try writer.print("'{s}'", .{data.value});
            },
            .Ident => |data| {
                try writer.print("_{s}", .{data.name});
            },
            .Paren => |data| {
                try writer.print("[{}]", .{data.expr.*});
            },
            .Create => |data| {
                try writer.print("[new {}]", .{data.kind.*});
            },
            .Operation => |data| {
                try writer.print("{s}(", .{@tagName(data.op)});
                for (data.values, 0..) |value, idx| {
                    if (idx != 0) {
                        try writer.print(" {}", .{value});
                        continue;
                    }
                    try writer.print("{}", .{value});
                }

                _ = try writer.write(")");
            },
            .Struct => |data| {
                _ = try writer.write("struct ");
                for (data.body) |item| {
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    try writer.print("{}", .{item});
                }
            },
            .Proc => |data| {
                _ = try writer.write("proc");
                for (data.in) |item| {
                    try writer.print(" {s}", .{item});
                }
                try writer.print(" -> {}", .{data.out});
                if (data.body) |body| {
                    _ = try writer.write(":");
                    for (body) |item| {
                        _ = try writer.write("\n");
                        for (0..fmtIndents) |_| {
                            _ = try writer.write("  ");
                        }
                        try writer.print("{}", .{item});
                    }
                }
            },
        }
    }
};

var fmtIndents: usize = 0;

pub const Statement = struct {
    const StatementKind = enum {
        Expression,
        Definition,
        Return,
        While,
        If,
    };

    data: union(StatementKind) {
        Expression: Expression,
        Definition: *Definition,
        Return: ?Expression,
        While: struct {
            check: Expression,
            body: []Statement,
        },
        If: struct {
            check: Expression,
            body: []Statement,
            bodyElse: ?[]Statement,
        },
    },

    pub fn format(
        self: Statement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        _ = fmt;

        try writer.print("{s}: ", .{@tagName(self.data)});

        fmtIndents += 1;
        switch (self.data) {
            .Expression => |data| {
                try writer.print("{}", .{data});
            },
            .Definition => |data| {
                try writer.print("{}", .{data});
            },
            .Return => |data| {
                try writer.print("{?}", .{data});
            },
            .If => |data| {
                try writer.print("{}", .{data.check});
                fmtIndents -= 1;
                _ = try writer.write("\n");
                for (0..fmtIndents) |_| {
                    _ = try writer.write("  ");
                }
                try writer.print("Then:", .{});
                fmtIndents += 1;
                for (data.body) |item| {
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    try writer.print("{}", .{item});
                }
                fmtIndents -= 1;
                if (data.bodyElse != null) {
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    _ = try writer.write("Else:");
                    fmtIndents += 1;
                    for (data.bodyElse.?) |item| {
                        _ = try writer.write("\n");
                        for (0..fmtIndents) |_| {
                            _ = try writer.write("  ");
                        }
                        try writer.print("{}", .{item});
                    }
                    fmtIndents -= 1;
                }
                fmtIndents += 1;
            },
            .While => |data| {
                fmtIndents -= 1;
                try writer.print("{}", .{data.check});
                _ = try writer.write("\n");
                for (0..fmtIndents) |_| {
                    _ = try writer.write("  ");
                }
                try writer.print("Do", .{});
                fmtIndents += 1;
                for (data.body) |item| {
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    try writer.print("{}", .{item});
                }
                fmtIndents -= 1;
                fmtIndents += 1;
            },
        }
        fmtIndents -= 1;
    }
};

pub const Definition = struct {
    name: []const u8,
    value: Expression,

    pub fn format(
        self: Definition,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print("{s}: _{s}", .{ self.name, self.value });
    }
};

pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    scanner: scanner.Scanner,
    current: scanner.Scanner.Token = undefined,
    prev: scanner.Scanner.Token = undefined,

    pub fn init(scn: scanner.Scanner, alloc: std.mem.Allocator) Parser {
        return .{
            .scanner = scn,
            .allocator = alloc,
        };
    }

    pub fn advance(self: *Self) parserError!void {
        self.prev = self.current;
        self.current = try self.scanner.nextToken();
    }

    pub fn match(self: *Self, kind: scanner.Scanner.TokenType) parserError!bool {
        if (self.current.kind != kind) return false;
        try self.advance();
        return true;
    }

    const ExpressionLevel = enum {
        Assignment,
        Or,
        And,
        Equality,
        Comparison,
        Term,
        Factor,
        Unary,
        Call,
        Primary,
    };

    pub fn unary(self: *Self, current: *Expression, sublevel: ExpressionLevel, ty: scanner.Scanner.TokenType, out: Expression.Operation) parserError!bool {
        if (try self.match(ty)) {
            var values = try self.allocator.alloc(Expression, 2);
            values[0] = current.*;
            values[1] = try self.parseExpression(sublevel);

            current.* = .{
                .data = .{
                    .Operation = .{
                        .op = out,
                        .values = values,
                    },
                },
            };
            return true;
        }
        return false;
    }

    pub fn binary(self: *Self, current: *Expression, sublevel: ExpressionLevel, ty: scanner.Scanner.TokenType, out: Expression.Operation) parserError!bool {
        if (try self.match(ty)) {
            var values = try self.allocator.alloc(Expression, 2);
            values[0] = current.*;
            values[1] = try self.parseExpression(sublevel);

            current.* = .{
                .data = .{
                    .Operation = .{
                        .op = out,
                        .values = values,
                    },
                },
                .line = self.current.line,
                .col = self.current.col,
            };
            return true;
        }
        return false;
    }

    pub fn parseExpression(self: *Self, level: ExpressionLevel) parserError!Expression {
        if (level == .Primary) {
            var result: Expression = switch (self.current.kind) {
                .EXTERN => {
                    try self.advance();

                    var in = try self.allocator.alloc([]const u8, 0);

                    while (!try self.match(.ARROW)) {
                        in = try self.allocator.realloc(in, in.len + 1);
                        in[in.len - 1] = self.current.lexeme;
                        try self.advance();
                    }

                    var out = try self.allocator.create(Expression);
                    out.* = try self.parseExpression(.Assignment);

                    return .{
                        .data = .{
                            .Proc = .{
                                .in = in,
                                .out = out,
                                .body = null,
                                .ext = true,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .VAR => {
                    try self.advance();
                    var kind = try self.allocator.create(Expression);
                    kind.* = try self.parseExpression(.Or);

                    return .{
                        .data = .{
                            .Create = .{
                                .kind = kind,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .PROC => {
                    try self.advance();

                    var in = try self.allocator.alloc([]const u8, 0);

                    while (!try self.match(.ARROW)) {
                        in = try self.allocator.realloc(in, in.len + 1);
                        in[in.len - 1] = self.current.lexeme;
                        try self.advance();
                    }

                    var out = try self.allocator.create(Expression);
                    out.* = try self.parseExpression(.Assignment);

                    var body = try self.allocator.alloc(Statement, 0);

                    if (!try self.match(.LEFT_BRACE)) return .{
                        .data = .{
                            .Proc = .{
                                .in = in,
                                .out = out,
                                .body = null,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };

                    while (!try self.match(.RIGHT_BRACE)) {
                        var stmt = try self.parseStatement();
                        body = try self.allocator.realloc(body, body.len + 1);
                        body[body.len - 1] = stmt;
                    }

                    return .{
                        .data = .{
                            .Proc = .{
                                .in = in,
                                .out = out,
                                .body = body,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .STRUCT => {
                    try self.advance();

                    if (!try self.match(.LEFT_BRACE)) return .{
                        .data = .{
                            .Ident = .{
                                .name = "class",
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };

                    var body = try self.allocator.alloc(Definition, 0);

                    while (!try self.match(.RIGHT_BRACE)) {
                        var def = try self.parseDef();
                        body = try self.allocator.realloc(body, body.len + 1);
                        body[body.len - 1] = def;
                    }

                    return .{
                        .data = .{
                            .Struct = .{
                                .body = body,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .DOLLAR => {
                    try self.advance();

                    if (try self.match(.LEFT_BRACKET)) {
                        var kind = try self.parseExpression(.Assignment);

                        if (!try self.match(.COLON)) return error.ExpectedColon;

                        var params = try self.allocator.alloc(Expression, 1);
                        params[0] = kind;

                        while (!try self.match(.RIGHT_BRACKET)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_BRACKET)) {
                                    std.log.info("{any}", .{params});
                                    return error.ExpectedParen;
                                }
                                break;
                            }
                        }

                        var result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .ConstArray,
                                    .values = params,
                                },
                            },
                            .line = self.current.line,
                            .col = self.current.col,
                        };

                        return result;
                    } else if (try self.match(.LEFT_BRACE)) {
                        var params = try self.allocator.alloc(Expression, 0);

                        while (!try self.match(.RIGHT_BRACE)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_BRACE)) {
                                    std.log.info("{any}", .{params});
                                    return error.ExpectedParen;
                                }
                                break;
                            }
                        }

                        var result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .ConstOpaque,
                                    .values = params,
                                },
                            },
                            .line = self.current.line,
                            .col = self.current.col,
                        };

                        return result;
                    }

                    return error.BadDollarExpr;
                },
                .FLOAT => .{
                    .data = .{
                        .ConstFloat = .{
                            .value = try std.fmt.parseFloat(f64, self.current.lexeme),
                        },
                    },
                    .line = self.current.line,
                    .col = self.current.col,
                },
                .NUMBER => .{
                    .data = .{
                        .ConstInt = .{
                            .value = try std.fmt.parseInt(usize, self.current.lexeme, 0),
                        },
                    },
                    .line = self.current.line,
                    .col = self.current.col,
                },
                .STRING => .{
                    .data = .{
                        .ConstString = .{
                            .value = self.current.lexeme,
                        },
                    },
                    .line = self.current.line,
                    .col = self.current.col,
                },
                .CHAR => .{
                    .data = .{
                        .ConstInt = .{
                            .value = switch (self.current.lexeme.len) {
                                1 => self.current.lexeme[0],
                                2 => if (self.current.lexeme[0] == '\\') switch (self.current.lexeme[1]) {
                                    't' => '\t',
                                    'n' => '\n',
                                    'r' => '\r',
                                    'e' => '\x1b',
                                    else => self.current.lexeme[1],
                                } else return error.InvalidCharacter,
                                else => return error.InvalidCharacter,
                            },
                        },
                    },
                    .line = self.current.line,
                    .col = self.current.col,
                },
                .IDENTIFIER => .{
                    .data = .{
                        .Ident = .{
                            .name = self.current.lexeme,
                        },
                    },
                    .line = self.current.line,
                    .col = self.current.col,
                },
                .LEFT_PAREN => {
                    try self.advance();

                    var expr = try self.allocator.create(Expression);
                    expr.* = try self.parseExpression(.Assignment);

                    if (!try self.match(.RIGHT_PAREN)) return error.ExpectedParen;

                    return Expression{
                        .data = .{
                            .Paren = .{
                                .expr = expr,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                else => {
                    std.log.info("{s}", .{@tagName(self.current.kind)});
                    return error.InvalidChar;
                },
            };

            try self.advance();

            return result;
        } else if (level == .Unary) {
            const next = @intToEnum(ExpressionLevel, @enumToInt(level) + 1);
            switch (self.current.kind) {
                .STAR => {
                    try self.advance();

                    var values = try self.allocator.alloc(Expression, 1);
                    values[0] = try self.parseExpression(next);

                    return .{
                        .data = .{
                            .Operation = .{
                                .op = .Deref,
                                .values = values,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .TILDE => {
                    try self.advance();

                    var values = try self.allocator.alloc(Expression, 1);
                    values[0] = try self.parseExpression(next);

                    return .{
                        .data = .{
                            .Operation = .{
                                .op = .BitNot,
                                .values = values,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                else => {},
            }
        }

        const next = @intToEnum(ExpressionLevel, @enumToInt(level) + 1);

        var result = try self.parseExpression(next);

        switch (level) {
            .Assignment => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .EQUAL, .Assign);
                }
            },
            .And => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .AMPERSAND, .BitAnd);
                }
            },
            .Or => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .BAR, .BitOr);
                }
            },
            .Factor => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .PERCENT, .Mod);
                    added = added or try self.binary(&result, next, .SLASH, .Div);
                    added = added or try self.binary(&result, next, .STAR, .Mul);
                }
            },
            .Equality => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .EXCLAIM_EQUAL, .NotEqual);
                    added = added or try self.binary(&result, next, .EQUAL_EQUAL, .Equal);
                }
            },
            .Comparison => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .LT, .Less);
                    added = added or try self.binary(&result, next, .GT, .Greater);
                }
            },
            .Term => {
                var added = true;
                while (added) {
                    added = false;

                    added = added or try self.binary(&result, next, .PLUS, .Add);
                    added = added or try self.binary(&result, next, .MINUS, .Sub);
                }
            },
            .Call => {
                var added = true;
                while (added) {
                    added = false;

                    if (self.current.kind == .LEFT_BRACKET) {
                        try self.advance();

                        var params = try self.allocator.alloc(Expression, 1);
                        params[0] = result;

                        while (!try self.match(.RIGHT_BRACKET)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_BRACKET)) return error.ExpectedParen;
                                break;
                            }
                        }

                        result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .IndexAccess,
                                    .values = params,
                                },
                            },
                            .line = self.current.line,
                            .col = self.current.col,
                        };

                        added = true;
                    } else if (self.current.kind == .LEFT_PAREN) {
                        try self.advance();

                        var params = try self.allocator.alloc(Expression, 1);
                        params[0] = result;

                        while (!try self.match(.RIGHT_PAREN)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedParen;
                                break;
                            }
                        }

                        result = .{
                            .data = .{
                                .Operation = .{
                                    .op = .Call,
                                    .values = params,
                                },
                            },
                            .line = self.current.line,
                            .col = self.current.col,
                        };

                        added = true;
                    }

                    added = added or try self.binary(&result, next, .DOT, .Access);
                }
            },
            else => {},
        }

        return result;
    }

    pub fn parseStatement(self: *Self) parserError!Statement {
        switch (self.current.kind) {
            .DEF => {
                var def = try self.allocator.create(Definition);
                def.* = try self.parseDef();

                return .{
                    .data = .{
                        .Definition = def,
                    },
                };
            },
            .RET => {
                try self.advance();

                if (try self.match(.SEMI_COLON)) return .{
                    .data = .{
                        .Return = null,
                    },
                };

                var expr = try self.parseExpression(.Assignment);

                if (self.prev.kind != .RIGHT_BRACE and !try self.match(.SEMI_COLON)) {
                    std.log.info("{}", .{expr});

                    return error.ExpectedSemicolon;
                }

                return .{
                    .data = .{
                        .Return = expr,
                    },
                };
            },
            .WHILE => {
                try self.advance();

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var check = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.alloc(Statement, 0);

                if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                while (!try self.match(.RIGHT_BRACE)) {
                    var stmt = try self.parseStatement();
                    body = try self.allocator.realloc(body, body.len + 1);
                    body[body.len - 1] = stmt;
                }

                return .{
                    .data = .{
                        .While = .{
                            .check = check,
                            .body = body,
                        },
                    },
                };
            },
            .IF => {
                try self.advance();

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var check = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.alloc(Statement, 0);

                if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                while (!try self.match(.RIGHT_BRACE)) {
                    var stmt = try self.parseStatement();
                    body = try self.allocator.realloc(body, body.len + 1);
                    body[body.len - 1] = stmt;
                }

                var bodyElse: ?[]Statement = null;

                if (try self.match(.ELSE)) {
                    bodyElse = try self.allocator.alloc(Statement, 0);

                    if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                    while (!try self.match(.RIGHT_BRACE)) {
                        var stmt = try self.parseStatement();
                        bodyElse = try self.allocator.realloc(bodyElse.?, bodyElse.?.len + 1);
                        bodyElse.?[bodyElse.?.len - 1] = stmt;
                    }
                }

                return .{
                    .data = .{
                        .If = .{
                            .check = check,
                            .body = body,
                            .bodyElse = bodyElse,
                        },
                    },
                };
            },
            else => {
                var expr = try self.parseExpression(.Assignment);

                if (self.prev.kind != .RIGHT_BRACE and !try self.match(.SEMI_COLON)) {
                    std.log.info("{} {} {}", .{ expr, self.current.line, self.current.col });

                    return error.ExpectedSemicolon;
                }

                return .{
                    .data = .{
                        .Expression = expr,
                    },
                };
            },
        }
    }

    pub fn parseDef(self: *Self) parserError!Definition {
        if (!try self.match(.DEF)) return error.ExpectedDef;
        var name = self.current.lexeme;
        try self.advance();

        if (!try self.match(.COLON)) return error.ExpectedColon;

        var val = try self.parseExpression(.Assignment);

        if (self.prev.kind != .RIGHT_BRACE and !try self.match(.SEMI_COLON)) {
            std.log.info("{}", .{val});

            return error.ExpectedSemicolon;
        }

        return .{
            .name = name,
            .value = val,
        };
    }

    pub fn parse(self: *Self) ![]Definition {
        var result = try self.allocator.alloc(Definition, 0);

        try self.advance();
        while (self.parseDef() catch |err| blk: {
            if (!try self.match(.EOF)) return err;
            break :blk null;
        }) |def| {
            result = try self.allocator.realloc(result, result.len + 1);
            result[result.len - 1] = def;
        }

        return result;
    }
};
