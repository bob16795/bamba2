const std = @import("std");

const interpreter = @import("interpreter.zig");
const parser = @import("parser.zig");

const Value = interpreter.Interpreter.Value;

pub const OperationError = error{
    InvalidParams,
    Todo,
};

pub const IMPLS = [_]parser.Expression.Operation{
    .Add,
    .Sub,
    .BitNot,
    .BitAnd,
    .BitOr,
    .Mul,
    .Div,
    .Less,
    .Equal,
    .NotEqual,
};

const defaultPrefix = interpreter.defaultPrefix;

pub fn operationAddRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildAdd(a.Value.val, b.Value.val, defaultPrefix ++ "Add"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationAdd(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a + b,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .ConstReal = a + b,
                },
                .ConstInt => |b| return .{
                    .ConstReal = a + @as(f64, @floatFromInt(b)),
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationAddRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationAddRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationAddRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationAddRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationMulRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildMul(a.Value.val, b.Value.val, defaultPrefix ++ "Mul"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationMul(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a * b,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .ConstReal = a * b,
                },
                .ConstInt => |b| return .{
                    .ConstReal = a * @as(f64, @floatFromInt(b)),
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationMulRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationMulRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationMulRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationMulRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

const boolType: interpreter.Interpreter.Value = .{
    .IntType = .{
        .size = 1,
        .signed = false,
    },
};

pub fn operationEqlRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildICmp(.EQ, a.Value.val, b.Value.val, defaultPrefix ++ "Eql"),
                        .kind = &boolType,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        .PtrType => {
            switch (b.Value.kind.*) {
                .PtrType => return .{
                    .Value = .{
                        .val = int.builder.buildICmp(.EQ, a.Value.val, b.Value.val, defaultPrefix ++ "Eql"),
                        .kind = &boolType,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationEql(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .Builtin = if (a == b) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .Builtin = if (a == b) .TrueValue else .FalseValue,
                },
                .ConstInt => |b| return .{
                    .Builtin = if (a == @as(f64, @floatFromInt(b))) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationEqlRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationEqlRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationEqlRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationEqlRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        .FunctionType => {
            switch (vals[1]) {
                .FunctionType => {
                    if (vals[0].FunctionType.in.len !=
                        vals[1].FunctionType.in.len) return .{
                        .Builtin = .FalseValue,
                    };

                    return .{
                        .Builtin = .TrueValue,
                    };
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationLessRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildICmp(.ULT, a.Value.val, b.Value.val, defaultPrefix ++ "Less"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationLess(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .Builtin = if (a < b) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .Builtin = if (a < b) .TrueValue else .FalseValue,
                },
                .ConstInt => |b| return .{
                    .Builtin = if (a < @as(f64, @floatFromInt(b))) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationLessRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationLessRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationLessRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationLessRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationAndRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildAnd(a.Value.val, b.Value.val, defaultPrefix ++ "And"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationAnd(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a & b,
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationAndRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationAndRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationAndRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationAndRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationOrRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildOr(a.Value.val, b.Value.val, defaultPrefix ++ "Or"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationOr(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a | b,
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationOrRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationOrRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationOrRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationOrRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationDivRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildSDiv(a.Value.val, b.Value.val, defaultPrefix ++ "Mul"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationDiv(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a * b,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .ConstReal = a * b,
                },
                .ConstInt => |b| return .{
                    .ConstReal = a * @as(f64, @floatFromInt(b)),
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationDivRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationDivRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationDivRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationDivRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationNotRT(int: *interpreter.Interpreter, a: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => return .{
            .Value = .{
                .val = int.builder.buildNot(a.Value.val, defaultPrefix ++ "Not"),
                .kind = a.Value.kind,
            },
        },
        else => return error.InvalidParams,
    }
}

pub fn operationNot(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| return .{
            .ConstInt = ~a,
        },
        .Value => return operationNotRT(int, vals[0]),
        else => return error.InvalidParams,
    }
}

pub fn operationSubRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildSub(a.Value.val, b.Value.val, defaultPrefix ++ "Sub"),
                        .kind = a.Value.kind,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationSub(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .ConstInt = a - b,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .ConstReal = a - b,
                },
                .ConstInt => |b| return .{
                    .ConstReal = a - @as(f64, @floatFromInt(b)),
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationSubRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationSubRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationSubRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationSubRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationNotEqlRT(int: *interpreter.Interpreter, a: Value, b: Value) OperationError!Value {
    switch (a.Value.kind.*) {
        .IntType => {
            switch (b.Value.kind.*) {
                .IntType => return .{
                    .Value = .{
                        .val = int.builder.buildICmp(.NE, a.Value.val, b.Value.val, defaultPrefix ++ "NEql"),
                        .kind = &boolType,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        .PtrType => {
            switch (b.Value.kind.*) {
                .PtrType => return .{
                    .Value = .{
                        .val = int.builder.buildICmp(.NE, a.Value.val, b.Value.val, defaultPrefix ++ "NEql"),
                        .kind = &boolType,
                    },
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}

pub fn operationNotEql(
    int: *interpreter.Interpreter,
    vals: []Value,
) OperationError!Value {
    switch (vals[0]) {
        .ConstInt => |a| {
            switch (vals[1]) {
                .ConstInt => |b| return .{
                    .Builtin = if (a != b) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .ConstReal => |a| {
            switch (vals[1]) {
                .ConstReal => |b| return .{
                    .Builtin = if (a != b) .TrueValue else .FalseValue,
                },
                .ConstInt => |b| return .{
                    .Builtin = if (a != @as(f64, @floatFromInt(b))) .TrueValue else .FalseValue,
                },
                else => return error.InvalidParams,
            }
        },
        .Ptr => {
            var a = vals[0].toValue(int) orelse unreachable;
            switch (vals[1]) {
                .Value => {
                    return operationNotEqlRT(int, a, vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationNotEqlRT(int, a, b);
                },
                else => return error.InvalidParams,
            }
        },
        .Value => {
            switch (vals[1]) {
                .Value => {
                    return operationNotEqlRT(int, vals[0], vals[1]);
                },
                .Ptr => {
                    var b = vals[1].toValue(int) orelse unreachable;
                    return operationNotEqlRT(int, vals[0], b);
                },
                else => return error.InvalidParams,
            }
        },
        .FunctionType => {
            switch (vals[1]) {
                .FunctionType => {
                    if (vals[0].FunctionType.in.len !=
                        vals[1].FunctionType.in.len) return .{
                        .Builtin = .TrueValue,
                    };

                    return .{
                        .Builtin = .FalseValue,
                    };
                },
                else => return error.InvalidParams,
            }
        },
        else => return error.InvalidParams,
    }
}
