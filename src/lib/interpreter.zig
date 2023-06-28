const std = @import("std");
const parser = @import("parser.zig");
const llvm = @import("llvm.zig");

const InterpreterError = error{
    DumnDev,
    OutOfMemory,
    InvalidCall,
    InvalidNew,
    InvalidType,
    InvalidValue,
    InvalidAssign,
    NotFound,
    NoReturn,
    BadCast,
};

pub const Interpreter = struct {
    const Self = @This();

    const NodeStatus = enum { Visited, Unvisited };
    const NodeValue = enum {
        ConstInt,
        ConstReal,
        ConstString,
        IntType,
        RealType,
        PtrType,
        FunctionType,
        FunctionData,
        Value,
        Ptr,
        Prop,
        Struct,
        Builtin,
    };

    const Builtin = enum {
        NullValue,
        TrueValue,
        FalseValue,

        VoidType,
        ClassType,
        StringType,

        ErrorFn,
    };

    const Value = union(NodeValue) {
        ConstInt: usize,
        ConstReal: f64,
        ConstString: []const u8,
        Value: struct {
            val: *const llvm.Value,
            kind: *const Value,
        },
        Ptr: struct {
            val: *const llvm.Value,
            kind: *const Value,
        },
        PtrType: struct {
            child: *const Value,
        },
        IntType: u32,
        RealType: u34,
        FunctionType: struct {
            in: [][]const u8,
            out: *Value,
        },
        FunctionData: struct {
            kind: struct {
                in: [][]const u8,
                out: *Value,
            },
            context: InterpreterContext,
            conts: ?[]const parser.Statement,
            ext: bool,
        },
        Struct: struct {
            kind: *const llvm.Type,
            values: std.StringHashMap(InterpreterNode),
            context: InterpreterContext,
            parent: ?*Value,
        },
        Prop: struct {
            idx: usize,
            kind: *Value,
        },
        Builtin: Builtin,

        pub fn isTrue(self: *const Value) ?bool {
            switch (self.*) {
                .Builtin => |b| {
                    switch (b) {
                        .FalseValue => return false,
                        .TrueValue => return true,
                        else => return null,
                    }
                },
                else => return null,
            }
        }

        pub fn setTo(self: *const Value, parent: *Interpreter, value: *const Value) !Value {
            switch (self.*) {
                .Ptr => |val| {
                    if (value.* != .Value) return error.InvalidAssign;
                    // TODO: check type
                    _ = parent.builder.buildStore(val.val, value.Value.val);
                },
                else => return error.InvalidAssign,
            }
            return self.*;
        }

        pub fn getString(self: *const Value) ?[]const u8 {
            switch (self.*) {
                .ConstString => |v| return v,
                else => return null,
            }
        }
        pub fn getValue(self: *const Value, parent: *Interpreter) ?*const llvm.Value {
            switch (self.*) {
                .Value => |v| {
                    return v.val;
                },
                .Ptr => |v| {
                    return parent.builder.buildLoad(v.kind.getType(parent) orelse unreachable, v.val, "load");
                },
                .ConstString => |s| {
                    if (parent.strings.getPtr(s)) |res| return res.Value.val;
                    var adds = parent.context.constString(s.ptr, @intCast(c_uint, s.len), .False);

                    parent.strings.put(s, .{ .Value = .{ .val = adds, .kind = &.{ .Builtin = .StringType } } }) catch unreachable;

                    return adds;
                },
                else => return null,
            }
        }

        pub fn getType(self: *const Value, parent: *Interpreter) ?*const llvm.Type {
            switch (self.*) {
                .IntType => |t| return parent.context.intType(t),
                .Struct => |t| return t.kind,
                .Builtin => |b| {
                    switch (b) {
                        .VoidType => return parent.context.voidType(),
                        else => return null,
                    }
                },
                else => return null,
            }
        }

        pub fn format(
            self: Value,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            _ = fmt;
            switch (self) {
                .Struct => {
                    var iter = self.Struct.values.iterator();
                    while (iter.next()) |item| {
                        try writer.print("{s}: {}, ", .{ item.key_ptr.*, item.value_ptr.* });
                    }
                },
                else => return,
            }
        }
    };

    const InterpreterNode = union(NodeStatus) {
        Visited: struct {
            parent: ?*Value,
            value: Value,
        },
        Unvisited: struct {
            ctx: InterpreterContext,
            value: *const parser.Expression,
        },

        pub fn format(
            self: InterpreterNode,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            _ = fmt;
            switch (self) {
                .Unvisited => {
                    try writer.print("UNVISITED: {}", .{self.Unvisited.value});
                },
                .Visited => {
                    try writer.print("VISITED: {}", .{self.Visited.value});
                },
            }
        }
    };

    const InterpreterContext = struct {
        locals: std.StringHashMap(Value),
        function: ?*const llvm.Value = null,
        current: *InterpreterNode,

        pub fn dupe(self: *const InterpreterContext, cur: *InterpreterNode) !InterpreterContext {
            return .{
                .locals = try self.locals.clone(),
                .function = self.function,
                .current = cur,
            };
        }
    };

    root: *InterpreterNode,
    strings: std.StringHashMap(Value),

    context: *llvm.Context,
    module: *llvm.Module,
    builder: *llvm.Builder,

    allocator: std.mem.Allocator,

    pub fn init(nodes: []parser.Definition, allocator: std.mem.Allocator) InterpreterError!Self {
        llvm.LLVMInitializeX86TargetInfo();
        llvm.LLVMInitializeX86Target();
        llvm.LLVMInitializeX86TargetMC();
        llvm.LLVMInitializeX86AsmParser();
        llvm.LLVMInitializeX86AsmPrinter();

        var ctx = llvm.Context.create();
        var mod = llvm.Module.createWithName("Context", ctx);
        var builder = ctx.createBuilder();

        var root = try allocator.create(InterpreterNode);
        root.* = InterpreterNode{
            .Visited = .{
                .parent = null,
                .value = .{
                    .Struct = .{
                        .values = std.StringHashMap(InterpreterNode).init(allocator),
                        .context = InterpreterContext{
                            .locals = std.StringHashMap(Value).init(allocator),
                            .current = root,
                        },
                        .parent = null,
                        .kind = undefined,
                    },
                },
            },
        };

        for (nodes) |*node| {
            try root.Visited.value.Struct.values.put(node.name, .{
                .Unvisited = .{
                    .value = &node.value,
                    .ctx = try root.Visited.value.Struct.context.dupe(root),
                },
            });
        }

        return .{
            .strings = std.StringHashMap(Value).init(allocator),
            .root = root,
            .allocator = allocator,
            .context = ctx,
            .module = mod,
            .builder = builder,
        };
    }

    pub fn getNode(self: *Self, parent: ?*InterpreterNode, child: []const u8) InterpreterError!*InterpreterNode {
        if (parent) |target| {
            switch (target.*) {
                .Unvisited => return try self.getNode(try self.visitNode(parent, &target.Unvisited.ctx.current.Visited.value), child),
                .Visited => {
                    switch (target.Visited.value) {
                        .Struct => {
                            return target.Visited.value.Struct.values.getPtr(child) orelse {
                                if (target.Visited.value.Struct.context.locals.getPtr(child)) |value| {
                                    var tmp = try self.allocator.create(InterpreterNode);
                                    tmp.* = .{
                                        .Visited = .{
                                            .value = value.*,
                                            .parent = &target.Visited.value,
                                        },
                                    };

                                    return tmp;
                                }
                                if (target.Visited.parent != null) {
                                    var tmp = try self.allocator.create(InterpreterNode);
                                    tmp.* = .{
                                        .Visited = .{
                                            .value = target.Visited.parent.?.*,
                                            .parent = target.Visited.parent.?.Struct.parent,
                                        },
                                    };

                                    return self.getNode(tmp, child);
                                    //var result = try self.allocator.create(InterpreterNode);
                                    //result.* = .{ .Visited = target.Visited.parent.?.Struct.values.get(child) orelse return error.NotFound };
                                    //return result;
                                }

                                std.log.info("child {s}", .{child});
                                return error.NotFound;
                            };
                        },
                        else => return error.DumnDev,
                    }
                },
            }
        } else {
            return try self.getNode(self.root, child);
        }
    }

    pub fn visitStmt(self: *Self, stmt: parser.Statement, ctx: *InterpreterContext) InterpreterError!?Value {
        switch (stmt.data) {
            .Expression => |e| {
                _ = try self.visitExpr(e, ctx);
            },
            .Return => |e| {
                var result = if (e) |expr| try self.visitExpr(expr, ctx) else Value{ .Builtin = .VoidType };
                return result;
            },
            .If => |e| {
                var val = try self.visitExpr(e.check, ctx);
                if (val.isTrue() orelse return error.InvalidValue) {
                    for (e.body) |node| {
                        if (try self.visitStmt(node, ctx)) |res| return res;
                    }
                } else {
                    if (e.bodyElse) |els| {
                        for (els) |node| {
                            if (try self.visitStmt(node, ctx)) |res| return res;
                        }
                    }
                }
            },
            else => return error.DumnDev,
        }

        return null;
    }

    pub fn implCall(self: *Self, func: *Value, params: []Value, ctx: *InterpreterContext) InterpreterError!Value {
        if (ctx.function != null) {
            if (func.FunctionData.ext) {
                var lParams = try self.allocator.alloc(*const llvm.Value, params.len);
                for (lParams, params) |*lp, p| {
                    lp.* = p.getValue(self) orelse return error.InvalidValue;
                }
            }

            return error.DumnDev;
        }

        if (func.FunctionData.conts == null) return error.InvalidCall;

        for (func.FunctionData.kind.in, params) |name, param| {
            try func.FunctionData.context.locals.put(name, param);
        }

        var iter = ctx.locals.iterator();

        while (iter.next()) |item| {
            try func.FunctionData.context.locals.put(item.key_ptr.*, item.value_ptr.*);
        }

        for (func.FunctionData.conts.?) |node| {
            if (try self.visitStmt(node, &func.FunctionData.context)) |res| return res;
        }

        return error.NoReturn;
    }

    pub fn implCast(self: *Self, value: Value, target: Value) !Value {
        switch (target) {
            .IntType => {
                switch (value) {
                    .ConstInt => |v| return .{
                        .Value = .{
                            .val = target.getType(self).?.constInt(v, .False),
                            .kind = &target,
                        },
                    },
                    else => return error.BadCast,
                }
            },
            else => return error.BadCast,
        }
    }

    pub fn visitExpr(self: *Self, expr: parser.Expression, ctx: *InterpreterContext) InterpreterError!Value {
        switch (expr.data) {
            .Operation => |oper| {
                const values = oper.values;
                switch (oper.op) {
                    .Call => {
                        var func = try self.visitExpr(values[0], ctx);
                        if (func.getType(self)) |_| {
                            var val = try self.visitExpr(values[1], ctx);
                            return self.implCast(val, func);
                        }

                        switch (func) {
                            .FunctionData => {
                                var params = try self.allocator.alloc(Value, values.len - 1);
                                for (params, 1..) |*val, idx| {
                                    val.* = try self.visitExpr(values[idx], ctx);
                                }

                                return self.implCall(&func, params, ctx);
                            },
                            .Builtin => |b| switch (b) {
                                .ErrorFn => {
                                    var val = try self.visitExpr(values[1], ctx);
                                    std.log.err("Compile error: {?s}", .{val.getString()});
                                    std.c.exit(1);
                                },
                                else => return error.InvalidCall,
                            },
                            else => {
                                std.log.info("{s}", .{@tagName(func)});
                                return error.InvalidCall;
                            },
                        }
                    },
                    .Access => {
                        var root = try self.allocator.create(Value);
                        root.* = try self.visitExpr(values[0], ctx);
                        switch (root.*) {
                            .Struct => return (try self.visitNode(root.Struct.values.getPtr(values[1].data.Ident.name) orelse return error.NotFound, root)).Visited.value,
                            .Value => |val| {
                                switch (val.kind.*) {
                                    .Struct => {
                                        return error.DumnDev;
                                    },
                                    else => return error.NotFound,
                                }
                            },
                            .Ptr => |val| {
                                std.log.info("{s}", .{@tagName(val.kind.*)});
                                return error.DumnDev;
                            },
                            else => return error.NotFound,
                        }
                    },
                    .Assign => {
                        var assignee = try self.visitExpr(values[0], ctx);
                        var value = try self.visitExpr(values[1], ctx);

                        return try assignee.setTo(self, &value);
                    },
                    .Ref => {
                        var value = try self.allocator.create(Value);
                        value.* = try self.visitExpr(values[0], ctx);

                        var kind = try self.allocator.create(Value);
                        kind.* = .{
                            .PtrType = .{
                                .child = value.Ptr.kind,
                            },
                        };

                        return .{
                            .Value = .{
                                .val = value.Ptr.val,
                                .kind = kind,
                            },
                        };
                    },
                    else => {
                        std.log.debug("visit {}", .{expr});
                        return error.DumnDev;
                    },
                }
            },
            .ConstString => |str| {
                return .{
                    .ConstString = str.value,
                };
            },
            .Ident => |ident| {
                if (std.mem.eql(u8, ident.name, "class"))
                    return .{ .Builtin = .ClassType };
                if (std.mem.eql(u8, ident.name, "ERROR"))
                    return .{ .Builtin = .ErrorFn };
                if (std.mem.eql(u8, ident.name, "void"))
                    return .{ .Builtin = .VoidType };
                if (std.mem.eql(u8, ident.name, "false"))
                    return .{ .Builtin = .FalseValue };
                if (std.mem.eql(u8, ident.name, "true"))
                    return .{ .Builtin = .TrueValue };

                if (ident.name[0] == 'i') {
                    if (std.fmt.parseInt(c_uint, ident.name[1..], 0) catch null) |val| {
                        return .{
                            .IntType = val,
                        };
                    }
                }

                if (ctx.locals.get(ident.name)) |local| {
                    return local;
                }

                if (self.getNode(ctx.current, ident.name) catch null) |local| {
                    return (try self.visitNode(local, ctx.current.Visited.parent)).Visited.value;
                }

                return (try self.visitNode(try self.getNode(null, ident.name), &ctx.current.Visited.value)).Visited.value;
            },
            .ConstInt => |val| {
                return .{
                    .ConstInt = val.value,
                };
            },
            .Proc => |proc| {
                if (proc.body) |body| {
                    var result = .{
                        .FunctionData = .{
                            .kind = .{
                                .in = try self.allocator.alloc([]const u8, proc.in.len),
                                .out = try self.allocator.create(Value),
                            },
                            .context = try ctx.dupe(ctx.current),
                            .conts = body,
                            .ext = false,
                        },
                    };

                    for (proc.in, result.FunctionData.kind.in) |*expres, *res| {
                        res.* = expres.*;
                    }

                    result.FunctionData.kind.out.* = try self.visitExpr(proc.out.*, &result.FunctionData.context);

                    return result;
                }
                if (proc.ext) {
                    var result = .{
                        .FunctionData = .{
                            .kind = .{
                                .in = try self.allocator.alloc([]const u8, proc.in.len),
                                .out = try self.allocator.create(Value),
                            },
                            .context = try ctx.dupe(ctx.current),
                            .conts = null,
                            .ext = true,
                        },
                    };

                    for (proc.in, result.FunctionData.kind.in) |*expres, *res| {
                        res.* = expres.*;
                    }

                    result.FunctionData.kind.out.* = try self.visitExpr(proc.out.*, &result.FunctionData.context);

                    return result;
                }

                var result = .{
                    .FunctionType = .{
                        .in = try self.allocator.alloc([]const u8, proc.in.len),
                        .out = try self.allocator.create(Value),
                    },
                };

                for (proc.in, result.FunctionType.in) |*expres, *res| {
                    res.* = expres.*;
                }

                result.FunctionType.out.* = try self.visitExpr(proc.out.*, ctx);

                return result;
            },
            .Paren => |data| {
                return self.visitExpr(data.expr.*, ctx);
            },
            .Struct => |cls| {
                var root = try self.allocator.create(InterpreterNode);
                root.* = .{
                    .Visited = .{
                        .value = Value{
                            .Struct = .{
                                .values = std.StringHashMap(InterpreterNode).init(self.allocator),
                                .context = try ctx.dupe(root),
                                .parent = &ctx.current.Visited.value,
                                .kind = self.context.voidType(),
                            },
                        },
                        .parent = &ctx.current.Visited.value,
                    },
                };

                try root.Visited.value.Struct.context.locals.put("Self", root.Visited.value);

                var propIdx: usize = 0;
                var props = std.ArrayList(*const llvm.Type).init(self.allocator);
                defer props.deinit();

                for (cls.body) |*def| {
                    if (def.value.data == .Prop) {
                        var kind = try self.allocator.create(Value);
                        kind.* = try self.visitExpr(def.value.data.Prop.kind.*, ctx);

                        try root.Visited.value.Struct.values.put(def.name, .{
                            .Visited = .{
                                .value = .{
                                    .Prop = .{
                                        .idx = propIdx,
                                        .kind = kind,
                                    },
                                },
                                .parent = &root.Visited.value,
                            },
                        });

                        try props.append(kind.getType(self) orelse return error.InvalidType);

                        propIdx += 1;
                        continue;
                    }
                    try root.Visited.value.Struct.values.put(def.name, .{
                        .Unvisited = .{
                            .value = &def.value,
                            .ctx = try root.Visited.value.Struct.context.dupe(root),
                        },
                    });
                }
                root.Visited.value.Struct.kind = self.context.structType(props.items.ptr, @intCast(c_uint, props.items.len), .False);

                return root.Visited.value;
            },
            .Create => |create| {
                if (ctx.function == null) return error.InvalidNew;
                var kind = try self.allocator.create(Value);
                kind.* = try self.visitExpr(create.kind.*, ctx);

                var lKind = kind.getType(self) orelse return error.InvalidType;
                return .{
                    .Ptr = .{
                        .val = self.builder.buildAlloca(lKind, "value"),
                        .kind = kind,
                    },
                };
            },
            else => {
                std.log.debug("visit {s}", .{@tagName(expr.data)});
                return error.DumnDev;
            },
        }
    }

    pub fn visitNode(self: *Self, ptr: ?*InterpreterNode, parent: ?*Value) InterpreterError!*InterpreterNode {
        if (ptr) |target| {
            switch (target.*) {
                .Visited => return target,
                .Unvisited => {
                    var result = try self.visitExpr(target.Unvisited.value.*, &target.Unvisited.ctx);

                    target.* = .{
                        .Visited = .{
                            .value = result,
                            .parent = parent,
                        },
                    };

                    return target;
                },
            }
        } else return self.visitNode(self.root, parent);
    }

    pub fn implStmt(self: *Self, stmt: *const parser.Statement, ctx: *InterpreterContext) InterpreterError!void {
        switch (stmt.data) {
            .Return => |ret| {
                if (ret) |val| {
                    _ = self.builder.buildRet((try self.visitExpr(val, ctx)).getValue(self) orelse return error.InvalidValue);
                } else {
                    _ = self.builder.buildRetVoid();
                }
            },
            .Definition => |def| {
                var val = try self.visitExpr(def.value, ctx);

                try ctx.locals.put(def.name, val);
            },
            .Expression => |expr| {
                _ = try self.visitExpr(expr, ctx);
            },
            else => |data| {
                std.debug.print("{}\n", .{data});
                return error.DumnDev;
            },
        }
    }

    pub fn implNode(self: *Self, ptr: ?*InterpreterNode, parent: ?*Value) InterpreterError!void {
        if (ptr) |target| {
            switch (target.*) {
                .Visited => {
                    switch (target.Visited.value) {
                        .FunctionData => |func| {
                            var argsTypes = try self.allocator.alloc(*const llvm.Type, func.kind.in.len);
                            for (func.kind.in.len) |_| {
                                return error.DumnDev;
                            }
                            var ctx = try func.context.dupe(func.context.current);
                            var out = func.kind.out.getType(self) orelse return error.InvalidType;
                            var functionType = llvm.functionType(out, argsTypes.ptr, @intCast(c_uint, argsTypes.len), .False);
                            var function = self.module.addFunction("func", functionType);

                            var oldBB = self.builder.getInsertBlock();
                            var bb = self.context.appendBasicBlock(function, "entry");
                            self.builder.positionBuilderAtEnd(bb);

                            ctx.function = function;
                            if (func.conts) |conts| {
                                for (conts) |impl| {
                                    try self.implStmt(&impl, &ctx);
                                }
                            }

                            self.builder.positionBuilderAtEnd(oldBB);

                            return;
                        },
                        else => {
                            std.debug.print("{}\n", .{target.Visited});
                            return error.DumnDev;
                        },
                    }
                },
                .Unvisited => return try self.implNode(try self.visitNode(ptr, parent), parent),
            }
        } else return self.implNode(self.root, parent);
    }
};
