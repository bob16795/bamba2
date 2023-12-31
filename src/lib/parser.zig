const std = @import("std");
const scanner = @import("scanner.zig");

pub const parserError = error{
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
        Comptime,
        Operation,
        Include,
        Struct,
        Paren,
        Prop,
        Proc,
    };

    pub const Operation = enum {
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
        Ref,
        Neg,

        // unknown
        ConstOpaque,
        ConstArray,
        Call,
    };

    pub const FunctionInput = struct {
        name: []const u8,
        kind: ?*Expression,
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
        Comptime: struct {
            stmt: *Statement,
        },
        Operation: struct {
            op: Operation,
            values: []Expression,
        },
        Include: []const u8,
        Struct: struct {
            inherit: []Expression,
            body: []Definition,
        },
        Paren: struct {
            expr: *Expression,
        },
        Prop: struct {
            kind: *Expression,
        },
        Proc: struct {
            in: []FunctionInput,
            out: *Expression,
            body: ?*Statement,
            ext: bool = false,
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
            .Include => |data| {
                try writer.print("Import \"{s}\"", .{data});
            },
            .Prop => |data| {
                try writer.print("P[{}]", .{data.kind});
            },
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
            .Comptime => |data| {
                try writer.print("[ctime {}]", .{data.stmt});
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
                    try writer.print(" {s}", .{item.name});
                    if (item.kind) |kind| {
                        try writer.print("({})", .{kind});
                    }
                }
                try writer.print(" -> {}", .{data.out});
                if (data.body) |body| {
                    _ = try writer.write(":");
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    try writer.print("{}", .{body});
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
        Block,
        While,
        For,
        If,
    };

    data: union(StatementKind) {
        Expression: Expression,
        Definition: *Definition,
        Return: ?Expression,
        Block: []Statement,
        While: struct {
            check: Expression,
            body: *Statement,
        },
        For: struct {
            vName: []const u8,
            list: Expression,
            body: *Statement,
        },
        If: struct {
            check: Expression,
            body: *Statement,
            bodyElse: ?*Statement,
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
                _ = try writer.write("\n");
                for (0..fmtIndents) |_| {
                    _ = try writer.write("  ");
                }
                try writer.print("{}", .{data.body});
                fmtIndents -= 1;
                if (data.bodyElse != null) {
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    _ = try writer.write("Else:");
                    fmtIndents += 1;
                    _ = try writer.write("\n");
                    for (0..fmtIndents) |_| {
                        _ = try writer.write("  ");
                    }
                    try writer.print("{}", .{data.bodyElse.?});
                    fmtIndents -= 1;
                }
                fmtIndents += 1;
            },
            .For => |data| {
                fmtIndents -= 1;
                try writer.print("{s}", .{data.vName});
                try writer.print("In", .{});
                try writer.print("{}\n", .{data.list});
                fmtIndents += 1;
                _ = try writer.write("\n");
                for (0..fmtIndents) |_| {
                    _ = try writer.write("  ");
                }
                try writer.print("{}", .{data.body});
                fmtIndents -= 1;
                fmtIndents += 1;
            },
            .Block => |data| {
                for (data) |s| {
                    try writer.print("{}\n", .{s});
                }
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
                _ = try writer.write("\n");
                for (0..fmtIndents) |_| {
                    _ = try writer.write("  ");
                }
                try writer.print("{}", .{data.body});
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

    force: bool,

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

                    var in = try self.allocator.alloc(Expression.FunctionInput, 0);

                    while (!try self.match(.ARROW)) {
                        in = try self.allocator.realloc(in, in.len + 1);
                        in[in.len - 1] = .{
                            .name = self.current.lexeme,
                            .kind = null,
                        };

                        if (try self.match(.LEFT_BRACKET)) {
                            var kind = try self.allocator.create(Expression);
                            kind.* = try self.parseExpression(.Assignment);
                            in[in.len - 1].kind = kind;
                        }
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
                .PROP => {
                    try self.advance();
                    var kind = try self.allocator.create(Expression);
                    kind.* = try self.parseExpression(.Or);

                    return .{
                        .data = .{
                            .Prop = .{
                                .kind = kind,
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
                .COMPTIME => {
                    try self.advance();
                    var stmt = try self.allocator.create(Statement);
                    stmt.* = try self.parseStatement();

                    return .{
                        .data = .{
                            .Comptime = .{
                                .stmt = stmt,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .PROC => {
                    try self.advance();

                    var in = try self.allocator.alloc(Expression.FunctionInput, 0);

                    while (!try self.match(.ARROW)) {
                        in = try self.allocator.realloc(in, in.len + 1);
                        in[in.len - 1] = .{
                            .name = self.current.lexeme,
                            .kind = null,
                        };

                        try self.advance();

                        if (try self.match(.LEFT_BRACKET)) {
                            var kind = try self.allocator.create(Expression);
                            kind.* = try self.parseExpression(.Assignment);
                            in[in.len - 1].kind = kind;
                            if (!try self.match(.RIGHT_BRACKET)) return error.ExpectedParen;
                        }
                    }

                    var out = try self.allocator.create(Expression);
                    out.* = try self.parseExpression(.Assignment);

                    var fbody: ?*Statement = null;

                    if (self.current.kind == .LEFT_BRACE) {
                        var body = try self.allocator.create(Statement);
                        body.* = try self.parseStatement();

                        fbody = body;
                    }

                    return .{
                        .data = .{
                            .Proc = .{
                                .in = in,
                                .out = out,
                                .body = fbody,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .STRUCT => {
                    try self.advance();

                    var params = try self.allocator.alloc(Expression, 0);

                    if (try self.match(.LEFT_PAREN)) {
                        while (!try self.match(.RIGHT_PAREN)) {
                            var param = try self.parseExpression(.Assignment);
                            params = try self.allocator.realloc(params, params.len + 1);

                            params[params.len - 1] = param;

                            if (!try self.match(.COMMA)) {
                                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedParen;
                                break;
                            }
                        }
                    }

                    if (!try self.match(.LEFT_BRACE)) return error.ExpectedBrace;

                    var body = try self.allocator.alloc(Definition, 0);

                    while (!try self.match(.RIGHT_BRACE)) {
                        var def = try self.parseDef();
                        body = try self.allocator.realloc(body, body.len + 1);
                        body[body.len - 1] = def;
                    }

                    return .{
                        .data = .{
                            .Struct = .{
                                .inherit = params,
                                .body = body,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .DOLLAR => {
                    try self.advance();

                    if (try self.match(.LEFT_BRACE)) {
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
                                    .op = .ConstArray,
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
                .EMBED => blk: {
                    try self.advance();

                    const conts = std.fs.cwd().readFileAlloc(self.allocator, self.current.lexeme, 10000) catch return error.FileError;

                    break :blk .{
                        .data = .{
                            .ConstString = .{
                                .value = conts,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                .IMPORT => blk: {
                    try self.advance();

                    break :blk .{
                        .data = .{
                            .Include = self.current.lexeme,
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
                else => {
                    std.log.info("{}", .{self.current});
                    return error.InvalidChar;
                },
            };

            try self.advance();

            return result;
        } else if (level == .Unary) {
            const next = @as(ExpressionLevel, @enumFromInt(@intFromEnum(level) + 1));
            switch (self.current.kind) {
                .AMPERSAND => {
                    try self.advance();

                    var values = try self.allocator.alloc(Expression, 1);
                    values[0] = try self.parseExpression(next);

                    return .{
                        .data = .{
                            .Operation = .{
                                .op = .Ref,
                                .values = values,
                            },
                        },
                        .line = self.current.line,
                        .col = self.current.col,
                    };
                },
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

        const next = @as(ExpressionLevel, @enumFromInt(@intFromEnum(level) + 1));

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
                                std.log.info("{any}", .{params});

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
            .FORCE => {
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

                if (!try self.match(.SEMI_COLON)) {
                    std.log.info("{}", .{expr});

                    return error.ExpectedSemicolon;
                }

                return .{
                    .data = .{
                        .Return = expr,
                    },
                };
            },
            .FOR => {
                try self.advance();
                var name = self.current.lexeme;
                try self.advance();

                if (!try self.match(.IN)) return error.ExpectedBrace;

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var list = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.create(Statement);
                body.* = try self.parseStatement();

                return .{
                    .data = .{
                        .For = .{
                            .vName = name,
                            .list = list,
                            .body = body,
                        },
                    },
                };
            },
            .WHILE => {
                try self.advance();

                if (!try self.match(.LEFT_PAREN)) return error.ExpectedBrace;

                var check = try self.parseExpression(.Assignment);

                if (!try self.match(.RIGHT_PAREN)) return error.ExpectedBrace;

                var body = try self.allocator.create(Statement);
                body.* = try self.parseStatement();

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

                var body = try self.allocator.create(Statement);
                body.* = try self.parseStatement();

                var fBodyElse: ?*Statement = null;

                if (try self.match(.ELSE)) {
                    var bodyElse = try self.allocator.create(Statement);
                    bodyElse.* = try self.parseStatement();

                    fBodyElse = bodyElse;
                }

                return .{
                    .data = .{
                        .If = .{
                            .check = check,
                            .body = body,
                            .bodyElse = fBodyElse,
                        },
                    },
                };
            },
            .LEFT_BRACE => {
                try self.advance();
                var body = try self.allocator.alloc(Statement, 0);

                while (!try self.match(.RIGHT_BRACE)) {
                    var stmt = try self.parseStatement();
                    body = try self.allocator.realloc(body, body.len + 1);
                    body[body.len - 1] = stmt;
                }

                return .{
                    .data = .{
                        .Block = body,
                    },
                };
            },
            else => {
                var expr = try self.parseExpression(.Assignment);

                if (!try self.match(.SEMI_COLON)) {
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
        const force = self.current.kind == .FORCE;

        if (!try self.match(.DEF) and !try self.match(.FORCE)) return error.ExpectedDef;
        var name = self.current.lexeme;
        try self.advance();

        if (!try self.match(.COLON)) return error.ExpectedColon;

        var val = try self.parseExpression(.Assignment);

        if (!try self.match(.SEMI_COLON)) {
            std.log.info("{}", .{val});
            std.log.info("{}", .{self.current});

            return error.ExpectedSemicolon;
        }

        return .{
            .name = name,
            .value = val,
            .force = force,
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
