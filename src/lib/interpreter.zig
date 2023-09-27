const std = @import("std");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const llvm = @import("llvm.zig");
const ops = @import("ops.zig");

const InterpreterError = ops.OperationError || parser.parserError || error{
    DumnDev,
    OutOfMemory,
    InvalidCall,
    InvalidNew,
    InvalidType,
    InvalidValue,
    InvalidAssign,
    FailedImport,
    NotFound,
    NoReturn,
    BadString,
    BadCast,
    BadNode,
    DoubleVisit,
};

pub const defaultPrefix = "Anon";

pub const Interpreter = struct {
    const Self = @This();

    const NodeStatus = enum { Bad, Visited, Visiting, Unvisited };
    const NodeValue = enum {
        ConstInt,
        ConstReal,
        ConstString,
        ConstArray,

        IntType,
        RealType,
        PtrType,
        ArrayType,
        Include,
        FunctionType,
        FunctionData,
        Value,
        ValueMethod,

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
        TypeType,

        ErrorFn,
        AddDefFn,
        GetPropFn,
    };

    pub const FunctionDataType = struct {
        name: []const u8 = defaultPrefix,
        kind: struct {
            in: []parser.Expression.FunctionInput,
            out: *Value,
        },
        impls: std.StringHashMap(*const llvm.Value),
        implkind: ?*const llvm.Type,
        context: InterpreterContext,
        conts: ?*const parser.Statement,
        callKind: enum {
            Std,
            Ext,
            Ptr,
        },
    };

    pub const StructDataType = struct {
        name: []const u8 = defaultPrefix,
        kind: *const llvm.Type,
        values: std.StringHashMap(InterpreterNode),
        context: InterpreterContext,
        parent: ?*InterpreterNode,
    };

    pub const Value = union(NodeValue) {
        ConstInt: usize,
        ConstReal: f64,
        ConstString: []const u8,
        ConstArray: []Value,

        IntType: struct {
            signed: bool,
            size: u32,
        },
        RealType: u34,
        PtrType: struct {
            child: *const Value,
        },
        ArrayType: struct {
            child: *const Value,
            size: ?usize,
        },
        Include: struct {
            name: []const u8,
        },
        FunctionType: struct {
            fnkind: *const llvm.Type,
            kind: *const llvm.Type,
            in: []parser.Expression.FunctionInput,
            out: *Value,
        },
        FunctionData: *FunctionDataType,
        Value: struct {
            val: *const llvm.Value,
            kind: *const Value,
        },
        ValueMethod: struct {
            val: *const llvm.Value,
            kind: *const Value,
            func: *FunctionDataType,
        },
        Prop: struct {
            idx: ?usize,
            kind: *Value,
        },
        Struct: *StructDataType,
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

        pub fn getName(self: *Value) ?[]const u8 {
            return switch (self.*) {
                .FunctionData => self.FunctionData.name,
                .Struct => self.Struct.name,
                .Value => |*val| blk: {
                    const name = val.val.getValueName();
                    const len = std.mem.len(name);

                    break :blk name[0..len];
                },
                else => null,
            };
        }

        pub fn setName(self: *Value, name: []const u8) !void {
            switch (self.*) {
                .FunctionData => self.FunctionData.name = name,
                .Struct => self.Struct.name = name,
                .Value => |*val| val.val.setValueName2(@ptrCast(name), @intCast(name.len)),
                else => {},
            }
            return;
        }

        pub fn setTo(self: *Value, parent: *Interpreter, value: *Value) !Value {
            switch (self.*) {
                .Value => |val| {
                    switch (val.kind.*) {
                        .PtrType => {
                            var assign = value.getValue(parent) orelse {
                                std.log.info("{}", .{self});
                                std.log.info("{}", .{value});
                                return error.InvalidAssign;
                            };

                            // TODO: check type
                            _ = parent.builder.buildStore(assign, val.val);

                            return self.*;
                        },
                        else => {
                            std.log.info("{s}", .{@tagName(val.kind.*)});
                            return error.InvalidAssign;
                        },
                    }
                },
                .ConstInt => |*c| {
                    switch (value.*) {
                        .ConstInt => |val| {
                            c.* = val;

                            return self.*;
                        },
                        else => {
                            std.log.info("{s}", .{@tagName(value.*)});
                            return error.InvalidAssign;
                        },
                    }
                },
                else => return error.InvalidAssign,
            }
        }

        pub fn getString(self: *const Value) ?[]const u8 {
            switch (self.*) {
                .ConstString => |v| return v,
                else => return null,
            }
        }

        pub fn toValue(self: *const Value, parent: *Interpreter) ?Value {
            switch (self.*) {
                .Value => |v| {
                    switch (v.kind.*) {
                        .PtrType => |p| {
                            return .{
                                .Value = .{
                                    .val = parent.builder.buildLoad(p.child.getType(parent) orelse return null, v.val, defaultPrefix ++ "Load"),
                                    .kind = p.child,
                                },
                            };
                        },
                        else => {
                            return null;
                        },
                    }
                },
                else => return null,
            }
        }

        pub fn getValue(self: *Value, parent: *Interpreter) ?*const llvm.Value {
            switch (self.*) {
                .Value => |v| {
                    return v.val;
                },
                .Builtin => |v| {
                    return switch (v) {
                        .NullValue => parent.context.pointerType(0).constNull(),
                        else => null,
                    };
                },
                .ConstArray => |v| {
                    const vals = parent.allocator.alloc(*const llvm.Value, v.len) catch unreachable;
                    for (v, vals) |*value, *target| {
                        target.* = value.getValue(parent) orelse return null;
                    }

                    return parent.context.constStruct(
                        vals.ptr,
                        @intCast(vals.len),
                        .False,
                    );
                },
                .FunctionData => |_| {
                    return parent.implFunc(self, null, false) catch |err| {
                        std.log.info("{!}", .{err});
                        return null;
                    };
                },
                .ConstString => |s| {
                    if (parent.strings.getPtr(s)) |res| return res.Value.val;

                    var tmpStr = parent.allocator.dupe(u8, s) catch return null;
                    defer parent.allocator.free(tmpStr);

                    var finalStr = tmpStr;
                    finalStr.len = 0;
                    var idx: usize = 0;

                    while (idx < s.len) {
                        finalStr.len += 1;
                        finalStr[finalStr.len - 1] = s[idx];
                        if (s[idx] == '\\') {
                            idx += 1;
                            switch (s[idx]) {
                                'n' => finalStr[finalStr.len - 1] = '\n',
                                'b' => finalStr[finalStr.len - 1] = 8,
                                'r' => finalStr[finalStr.len - 1] = '\r',
                                't' => finalStr[finalStr.len - 1] = '\t',
                                else => |ch| finalStr[finalStr.len - 1] = ch,
                            }
                        }
                        idx += 1;
                    }

                    var str = parent.context.constString(finalStr.ptr, @as(c_uint, @intCast(finalStr.len)), .False);
                    var adds = parent.module.addGlobal(str.typeOf(), defaultPrefix ++ "string");
                    adds.setInitializer(str);

                    parent.strings.put(s, .{ .Value = .{ .val = adds, .kind = &.{ .Builtin = .StringType } } }) catch unreachable;

                    return adds;
                },
                else => {
                    std.log.info("{s}", .{@tagName(self.*)});

                    return null;
                },
            }
        }

        pub fn getType(self: *const Value, parent: *Interpreter) ?*const llvm.Type {
            switch (self.*) {
                .IntType => |t| return parent.context.intType(t.size),
                .RealType => |t| switch (t) {
                    32 => return parent.context.floatType(),
                    64 => return parent.context.doubleType(),
                    else => return null,
                },
                .Struct => |t| return t.kind,
                .Builtin => |b| {
                    switch (b) {
                        .VoidType => return parent.context.voidType(),
                        .TypeType => return parent.context.intType(1),
                        else => return null,
                    }
                },
                .ArrayType => |t| {
                    if (t.size) |size| {
                        return (t.child.getType(parent) orelse return null).arrayType(@intCast(size));
                    } else {
                        return (t.child.getType(parent) orelse return null).arrayType(0);
                    }
                },
                .FunctionType => |t| return t.kind,
                .PtrType => return parent.context.pointerType(0),
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
                .ConstInt => |v| try writer.print("Int({})", .{v}),
                .ConstReal => |v| try writer.print("Real({})", .{v}),
                .ConstString => |v| try writer.print("String('{s}')", .{v}),
                .ConstArray => |v| {
                    try writer.print("Arr(", .{});
                    for (v, 0..) |sub, idx| {
                        if (idx != 0)
                            try writer.print(", ", .{});
                        try writer.print("{}", .{sub});
                    }
                    try writer.print(")", .{});
                },
                .FunctionData => |v| {
                    try writer.print("Fn([", .{});
                    for (v.kind.in, 0..) |sub, idx| {
                        if (idx != 0)
                            try writer.print(", ", .{});
                        try writer.print("{?}", .{sub.kind});
                    }
                    try writer.print("], {?s})", .{v.kind.out.getName()});
                },
                .Value => |v| try writer.print("Value({})", .{v.kind}),
                .ValueMethod => |v| try writer.print("ValueMethod({}, {})", .{ v.kind, v.func }),

                .IntType => |v| try writer.print("TInt({})", .{v}),
                .RealType => |v| try writer.print("TReal({})", .{v}),
                .PtrType => |v| try writer.print("TPtr({})", .{v.child}),
                .ArrayType => |v| try writer.print("TArr({}, {?})", .{ v.child, v.size }),
                .FunctionType => |v| {
                    try writer.print("TFn([", .{});
                    for (v.in, 0..) |sub, idx| {
                        if (idx != 0)
                            try writer.print(", ", .{});
                        try writer.print("{?}", .{sub.kind});
                    }
                    try writer.print("], {})", .{v.out});
                },

                .Prop => |v| try writer.print("Prop({?}, {})", .{ v.idx, v.kind }),
                .Include => |v| try writer.print("Incl({s})", .{v.name}),
                .Struct => |v| {
                    try writer.print("Struct({s}, ", .{v.name});
                    var iter = v.values.iterator();
                    var idx: usize = 0;

                    while (iter.next()) |entry| : (idx += 1) {
                        if (idx != 0)
                            try writer.print(", ", .{});
                        try writer.print("{s}:{}", .{ entry.key_ptr.*, entry.value_ptr.* });
                    }

                    try writer.print(")", .{});
                },

                .Builtin => |v| try writer.print("Builtin({s})", .{@tagName(v)}),
            }
        }
    };

    const InterpreterNode = union(NodeStatus) {
        const VisitedImpl = struct {
            name: []const u8,
            parent: ?*InterpreterNode,
            value: Value,
        };

        Bad: u0,
        Visited: VisitedImpl,
        Visiting: []const u8,
        Unvisited: struct {
            name: []const u8,
            ctx: InterpreterContext,
            value: *const parser.Expression,
            force: bool = false,
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
                .Bad => try writer.print("BAD", .{}),
                .Visited => try writer.print("Visited[{}]", .{self.Visited.value}),
                .Unvisited => try writer.print("Unvisited[{*}]", .{self.Unvisited.value}),
                .Visiting => |v| try writer.print("Visiting[{s}]", .{v}),
            }
        }
    };

    pub const InterpreterContext = struct {
        locals: std.StringHashMap(Value),
        function: ?*const llvm.Value = null,
        current: *InterpreterNode,
        root: *InterpreterNode,

        pub fn format(
            self: InterpreterContext,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = options;
            _ = fmt;
            _ = self;
            _ = writer;
        }

        pub fn dupe(self: *const InterpreterContext, cur: *InterpreterNode) !InterpreterContext {
            return .{
                .locals = try self.locals.clone(),
                .function = self.function,
                .root = self.root,
                .current = cur,
            };
        }
    };

    root: *InterpreterNode,
    strings: std.StringHashMap(Value),
    includes: std.StringHashMap(InterpreterNode),

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

        const root = try allocator.create(InterpreterNode);
        const structData = try allocator.create(StructDataType);
        structData.* = .{
            .values = std.StringHashMap(InterpreterNode).init(allocator),
            .context = InterpreterContext{
                .locals = std.StringHashMap(Value).init(allocator),
                .current = root,
                .root = root,
            },
            .parent = null,
            .kind = ctx.voidType(),
        };

        root.* = InterpreterNode{
            .Visited = .{
                .name = "AnonRoot",
                .parent = null,
                .value = .{
                    .Struct = structData,
                },
            },
        };

        for (nodes) |*node| {
            try root.Visited.value.Struct.values.put(node.name, .{
                .Unvisited = .{
                    .name = node.name,
                    .value = &node.value,
                    .ctx = try root.Visited.value.Struct.context.dupe(root),
                    .force = node.force,
                },
            });
        }

        var int = Self{
            .strings = std.StringHashMap(Value).init(allocator),
            .includes = std.StringHashMap(InterpreterNode).init(allocator),
            .root = root,
            .allocator = allocator,
            .context = ctx,
            .module = mod,
            .builder = builder,
        };

        var tmp = [_]u8{0} ** 1000;

        for (nodes) |*pnode| {
            if (!pnode.force) continue;

            const vnode = (try int.visitNode(try int.getNode(int.root, pnode.name), &root.Visited.value));
            _ = int.implFunc(&vnode.Visited.value, null, true) catch |err| {
                _ = int.module.printModuleToFile("ir", @ptrCast(@alignCast(&tmp)));

                return err;
            };

            try root.Visited.value.Struct.values.put(pnode.name, vnode.*);
        }

        return int;
    }

    pub fn getProp(self: *Self, root: *Value, child: []const u8) !Value {
        if (std.mem.eql(u8, child, "SIZE")) {
            var sizeType = try self.allocator.create(Value);
            sizeType.* = .{
                .IntType = .{
                    .size = 64,
                    .signed = false,
                },
            };

            const gep = self.builder.buildStructGEP(
                (root.getType(self) orelse return error.InvalidValue).arrayType(0),
                self.context.pointerType(0).constNull(),
                @intCast(1),
                defaultPrefix ++ "SizePtr",
            );

            return .{
                .Value = .{
                    .val = self.builder.buildPtrToInt(
                        gep,
                        self.context.intType(64),
                        defaultPrefix ++ "Size",
                    ),
                    .kind = sizeType,
                },
            };
        }
        switch (root.*) {
            .FunctionData => |fnc| {
                if (std.mem.eql(u8, child, "TYPE")) {
                    return .{
                        .FunctionType = .{
                            .in = fnc.kind.in,
                            .out = fnc.kind.out,
                            .kind = undefined,
                            .fnkind = undefined,
                        },
                    };
                }
                return error.NotFound;
            },
            .Struct => |str| {
                _ = str;
                return (try self.visitNode(root.Struct.values.getPtr(child) orelse {
                    std.log.info("{s}", .{child});

                    return error.NotFound;
                }, root)).Visited.value;
            },
            .Value => |val| {
                if (std.mem.eql(u8, child, "TYPE")) {
                    return val.kind.*;
                }

                switch (val.kind.*) {
                    .Struct => |str| {
                        const v = str.values.getPtr(child) orelse {
                            std.log.info("{s}", .{child});

                            return error.NotFound;
                        };

                        var field = (try self.visitNode(v, root)).Visited.value;
                        if (field == .FunctionData) {
                            return .{
                                .ValueMethod = .{
                                    .val = val.val,
                                    .kind = val.kind,
                                    .func = field.FunctionData,
                                },
                            };
                        }
                        return .{
                            .Value = .{
                                .val = self.builder.buildStructGEP(
                                    str.kind,
                                    root.getValue(self) orelse return error.InvalidValue,
                                    @intCast(field.Prop.idx.?),
                                    defaultPrefix ++ "prop",
                                ),
                                .kind = field.Prop.kind,
                            },
                        };
                    },
                    .PtrType => |ptr| {
                        switch (ptr.child.*) {
                            .Struct => |str| {
                                const v = str.values.getPtr(child) orelse {
                                    std.log.info("{s}", .{child});

                                    return error.NotFound;
                                };

                                var field = (try self.visitNode(v, root)).Visited.value;
                                if (field == .FunctionData) {
                                    return .{
                                        .ValueMethod = .{
                                            .val = val.val,
                                            .kind = val.kind,
                                            .func = field.FunctionData,
                                        },
                                    };
                                }

                                var kind = try self.allocator.create(Value);
                                kind.* = .{
                                    .PtrType = .{
                                        .child = field.Prop.kind,
                                    },
                                };

                                return .{
                                    .Value = .{
                                        .val = self.builder.buildStructGEP(
                                            str.kind,
                                            root.getValue(self) orelse return error.InvalidValue,
                                            @intCast(field.Prop.idx.?),
                                            defaultPrefix ++ "prop",
                                        ),
                                        .kind = kind,
                                    },
                                };
                            },
                            else => {
                                std.log.info("{s}, {s}", .{ @tagName(root.*), child });
                                return error.NotFound;
                            },
                        }
                    },
                    else => {
                        std.log.info("{s}, {s}", .{ @tagName(root.*), @tagName(val.kind.*) });
                        return error.NotFound;
                    },
                }
            },
            .Include => |val| {
                const incPtr = self.includes.getPtr(val.name) orelse unreachable;
                return (try self.visitNode(incPtr.Visited.value.Struct.values.getPtr(child) orelse {
                    std.log.info("{s}", .{val.name});

                    return error.NotFound;
                }, &incPtr.Visited.value)).Visited.value;
            },
            else => {
                std.log.info("{s}, {s}", .{ @tagName(root.*), child });
                return error.NotFound;
            },
        }
    }

    pub fn getNode(self: *Self, target: *InterpreterNode, child: []const u8) InterpreterError!*InterpreterNode {
        switch (target.*) {
            .Bad => return error.BadNode,
            .Unvisited => return try self.getNode(try self.visitNode(target, &target.Unvisited.ctx.current.Visited.value), child),
            .Visiting => return error.DoubleVisit,
            .Visited => {
                switch (target.Visited.value) {
                    .Struct => {
                        return target.Visited.value.Struct.values.getPtr(child) orelse {
                            if (target.Visited.value.Struct.context.locals.getPtr(child)) |value| {
                                var tmp = try self.allocator.create(InterpreterNode);
                                tmp.* = .{
                                    .Visited = .{
                                        .name = target.Visited.name,
                                        .value = value.*,
                                        .parent = target,
                                    },
                                };

                                return tmp;
                            }

                            if (target.Visited.parent != null) {
                                return self.getNode(target.Visited.parent.?, child);
                                //var result = try self.allocator.create(InterpreterNode);
                                //result.* = .{ .Visited = target.Visited.parent.?.Struct.values.get(child) orelse return error.NotFound };
                                //return result;
                            }

                            std.log.info("ERR: {any}", .{self.root});
                            var iter = self.includes.iterator();

                            while (iter.next()) |entry| {
                                std.log.info("{s}:{}", .{ entry.key_ptr.*, entry.value_ptr.* });
                            }
                            std.log.info("ERR: {s}", .{child});
                            return error.NotFound;
                        };
                    },
                    .Include => |i| {
                        return self.getNode(self.includes.getPtr(i.name).?, child);
                    },
                    else => return error.DumnDev,
                }
            },
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
                    if (try self.visitStmt(e.body.*, ctx)) |res| return res;
                } else {
                    if (e.bodyElse) |els| {
                        if (try self.visitStmt(els.*, ctx)) |res| return res;
                    }
                }
            },
            .Definition => |def| {
                var val = try self.visitExpr(def.value, ctx);

                try val.setName(def.name);

                try ctx.locals.put(def.name, val);
            },
            .For => |e| {
                var list = try self.visitExpr(e.list, ctx);

                for (0..list.ConstArray.len) |idx| {
                    try ctx.locals.put(e.vName, list.ConstArray[idx]);

                    if (try self.visitStmt(e.body.*, ctx)) |res| return res;
                }
            },
            .Block => |e| {
                for (e) |expr|
                    if (try self.visitStmt(expr, ctx)) |res| return res;
            },
            else => return error.DumnDev,
        }

        return null;
    }

    pub fn implCall(self: *Self, func: *Value, params: []Value, ctx: *InterpreterContext) InterpreterError!Value {
        if (ctx.function != null) {
            var lParams = try self.allocator.alloc(*const llvm.Value, params.len);
            for (lParams, params) |*lp, *p| {
                lp.* = p.getValue(self) orelse self.context.intType(1).constNull();
            }

            if (func.FunctionData.callKind == .Ext) {
                var returnKind = func.FunctionData.kind.out.getType(self) orelse return error.InvalidType;

                var resultfnKind = llvm.functionType(
                    returnKind,
                    undefined,
                    0,
                    .False,
                );

                var newName = try self.allocator.dupeZ(u8, func.FunctionData.name);
                defer self.allocator.free(newName);

                var resultfn =
                    self.module.getNamedFunction(newName.ptr) orelse
                    self.module.addFunction(newName.ptr, resultfnKind);

                resultfn.setFunctionCallConv(.C);

                var resultCall = self.builder.buildCall(
                    resultfnKind,
                    resultfn,
                    lParams.ptr,
                    @intCast(lParams.len),
                    defaultPrefix ++ "Call",
                );

                return .{
                    .Value = .{
                        .val = resultCall,
                        .kind = func.FunctionData.kind.out,
                    },
                };
            }

            if (func.FunctionData.callKind == .Ptr) {
                var newName = try self.allocator.dupeZ(u8, func.FunctionData.name);
                defer self.allocator.free(newName);

                var resultfn = self.builder.buildLoad(func.FunctionData.impls.get("()").?.typeOf(), func.FunctionData.impls.get("()").?, "Load");

                var resultCall = self.builder.buildCall(
                    func.FunctionData.implkind.?,
                    resultfn,
                    lParams.ptr,
                    @intCast(lParams.len),
                    defaultPrefix ++ "Call",
                );

                return .{
                    .Value = .{
                        .val = resultCall,
                        .kind = func.FunctionData.kind.out,
                    },
                };
            }

            var impl = try self.implFunc(func, params, false);

            var resultCall = self.builder.buildCall(
                func.FunctionData.implkind.?,
                impl,
                lParams.ptr,
                @intCast(lParams.len),
                defaultPrefix ++ "Call",
            );

            return .{
                .Value = .{
                    .val = resultCall,
                    .kind = func.FunctionData.kind.out,
                },
            };
        }

        if (func.FunctionData.conts == null) return error.InvalidCall;

        for (func.FunctionData.kind.in, params) |data, param| {
            try func.FunctionData.context.locals.put(data.name, param);
        }

        var iter = ctx.locals.iterator();

        while (iter.next()) |item| {
            try func.FunctionData.context.locals.put(item.key_ptr.*, item.value_ptr.*);
        }

        if (try self.visitStmt(func.FunctionData.conts.?.*, &func.FunctionData.context)) |res| return res;

        return error.NoReturn;
    }

    pub fn implCast(self: *Self, value: *Value, target: *Value) !Value {
        switch (target.*) {
            .IntType => {
                switch (value.*) {
                    .ConstInt => |v| return .{
                        .Value = .{
                            .val = target.getType(self).?.constInt(v, .False),
                            .kind = target,
                        },
                    },
                    .ConstReal => |v| return .{
                        .Value = .{
                            .val = target.getType(self).?.constInt(@intFromFloat(v), .False),
                            .kind = target,
                        },
                    },
                    .Builtin => |v| .{
                        switch (v) {
                            .FalseValue => return .{
                                .Value = .{
                                    .val = target.getType(self).?.constInt(0, .False),
                                    .kind = target,
                                },
                            },
                            .TrueValue => return .{
                                .Value = .{
                                    .val = target.getType(self).?.constInt(1, .False),
                                    .kind = target,
                                },
                            },
                            else => return error.BadCast,
                        },
                    },
                    .Value => |v| switch (v.kind.*) {
                        .IntType => return .{
                            .Value = .{
                                .val = self.builder.buildIntCast2(v.val, target.getType(self).?, .False, defaultPrefix ++ "IntCast"),
                                .kind = target,
                            },
                        },
                        .RealType => return .{
                            .Value = .{
                                .val = self.builder.buildFPToSI(v.val, target.getType(self).?, defaultPrefix ++ "IntCast"),
                                .kind = target,
                            },
                        },
                        else => return error.BadCast,
                    },
                    else => return error.BadCast,
                }
            },
            .RealType => {
                switch (value.*) {
                    .Value => |v| switch (v.kind.*) {
                        .IntType => return .{
                            .Value = .{
                                .val = self.builder.buildSIToFP(v.val, target.getType(self).?, defaultPrefix ++ "RealCast"),
                                .kind = target,
                            },
                        },
                        .RealType => return .{
                            .Value = .{
                                .val = self.builder.buildFPExt(v.val, target.getType(self).?, defaultPrefix ++ "RealCast"),
                                .kind = target,
                            },
                        },
                        else => return error.BadCast,
                    },
                    .ConstInt => |v| return .{
                        .Value = .{
                            .val = target.getType(self).?.constReal(@floatFromInt(v)),
                            .kind = target,
                        },
                    },
                    .ConstReal => |v| return .{
                        .Value = .{
                            .val = target.getType(self).?.constReal(@floatCast(v)),
                            .kind = target,
                        },
                    },
                    else => return error.BadCast,
                }
            },
            else => switch (value.*) {
                .ConstInt => |v| return .{
                    .Value = .{
                        .val = self.context.intType(64).constInt(@intCast(v), .False).constIntToPtr(self.context.pointerType(0)),
                        .kind = target,
                    },
                },
                else => return .{
                    .Value = .{
                        .val = value.getValue(self) orelse return error.BadCast,
                        .kind = target,
                    },
                },
            },
        }
    }

    pub fn visitExpr(self: *Self, expr: parser.Expression, ctx: *InterpreterContext) InterpreterError!Value {
        switch (expr.data) {
            .Comptime => |comp| {
                var tmpCtx = try ctx.dupe(ctx.current);
                tmpCtx.function = null;

                return (try self.visitStmt(comp.stmt.*, &tmpCtx)) orelse .{
                    .Builtin = .VoidType,
                };
            },
            .Operation => |oper| {
                const values = oper.values;
                var eValues = try self.allocator.alloc(Value, oper.values.len);
                if (std.mem.containsAtLeast(parser.Expression.Operation, &ops.IMPLS, 1, &.{oper.op})) {
                    for (eValues, oper.values) |*value, operValue| {
                        value.* = try self.visitExpr(operValue, ctx);
                    }
                }

                switch (oper.op) {
                    .BitNot => return try ops.operationNot(self, eValues),
                    .BitAnd => return try ops.operationAnd(self, eValues),
                    .BitOr => return try ops.operationOr(self, eValues),
                    .Add => return try ops.operationAdd(self, eValues),
                    .Sub => return try ops.operationSub(self, eValues),
                    .Equal => return try ops.operationEql(self, eValues),
                    .NotEqual => return try ops.operationNotEql(self, eValues),
                    .Less => return try ops.operationLess(self, eValues),
                    .Mul => return try ops.operationMul(self, eValues),
                    .Div => return try ops.operationDiv(self, eValues),
                    .Call => {
                        var func = try self.allocator.create(Value);
                        func.* = try self.visitExpr(values[0], ctx);
                        if (func.getType(self)) |_| {
                            var val = try self.allocator.create(Value);
                            val.* = try self.visitExpr(values[1], ctx);
                            return self.implCast(val, func);
                        }

                        switch (func.*) {
                            .ValueMethod => |vm| {
                                var params = try self.allocator.alloc(Value, values.len);
                                params[0] = .{
                                    .Value = .{
                                        .val = vm.val,
                                        .kind = vm.kind,
                                    },
                                };

                                for (params[1..], 1..) |*val, idx| {
                                    val.* = try self.visitExpr(values[idx], ctx);
                                }

                                var fd = .{
                                    .FunctionData = vm.func,
                                };

                                return self.implCall(&fd, params, ctx);
                            },
                            .FunctionData => {
                                var params = try self.allocator.alloc(Value, values.len - 1);
                                for (params, 1..) |*val, idx| {
                                    val.* = try self.visitExpr(values[idx], ctx);
                                }

                                return self.implCall(func, params, ctx);
                            },
                            .Builtin => |b| switch (b) {
                                .ErrorFn => {
                                    var val = try self.visitExpr(values[1], ctx);
                                    std.log.err("Compile error: {?s}", .{val.getString()});
                                    std.c.exit(1);
                                },
                                .AddDefFn => {
                                    var cls = try self.allocator.create(Value);
                                    cls.* = try self.visitExpr(values[1], ctx);
                                    var name = try self.allocator.create(Value);
                                    name.* = try self.visitExpr(values[2], ctx);
                                    var value = try self.visitExpr(values[3], ctx);

                                    const namev = name.getString() orelse return error.BadString;

                                    try value.setName(namev);

                                    try cls.Struct.values.put(namev, .{
                                        .Visited = .{
                                            .name = namev,
                                            .parent = @fieldParentPtr(
                                                InterpreterNode,
                                                "Visited",
                                                @fieldParentPtr(InterpreterNode.VisitedImpl, "value", cls),
                                            ),
                                            .value = value,
                                        },
                                    });

                                    if (value == .Prop) {
                                        var iter = cls.Struct.values.iterator();
                                        var propIdx: usize = 0;
                                        var props = std.ArrayList(*const llvm.Type).init(self.allocator);

                                        while (iter.next()) |kv| {
                                            if (kv.value_ptr.* != .Visited) continue;

                                            if (kv.value_ptr.Visited.value == .Prop) {
                                                var kind = kv.value_ptr.Visited.value.Prop.kind.*;
                                                kv.value_ptr.Visited.value.Prop.idx = propIdx;

                                                try props.append(kind.getType(self) orelse return error.InvalidType);

                                                propIdx += 1;
                                            }
                                        }

                                        cls.Struct.kind = self.context.structType(props.items.ptr, @intCast(props.items.len), .False);
                                    }

                                    return value;
                                },
                                .GetPropFn => {
                                    var root = try self.allocator.create(Value);
                                    root.* = try self.visitExpr(values[1], ctx);
                                    var name = try self.allocator.create(Value);
                                    name.* = try self.visitExpr(values[2], ctx);

                                    const namev = name.getString() orelse return error.BadString;

                                    return try self.getProp(root, namev);
                                },
                                else => return error.InvalidCall,
                            },
                            .Value => |b| switch (b.kind.*.PtrType.child.*) {
                                .FunctionType => |kind| {
                                    var params = try self.allocator.alloc(Value, values.len - 1);
                                    for (params, 1..) |*val, idx| {
                                        val.* = try self.visitExpr(values[idx], ctx);
                                    }

                                    var data = try self.allocator.create(FunctionDataType);

                                    data.* = .{
                                        .name = defaultPrefix ++ "FnData",
                                        .kind = .{
                                            .in = kind.in,
                                            .out = kind.out,
                                        },
                                        .impls = std.StringHashMap(*const llvm.Value).init(self.allocator),
                                        .implkind = kind.fnkind,
                                        .context = try ctx.dupe(ctx.current),
                                        .callKind = .Ptr,
                                        .conts = null,
                                    };

                                    try data.impls.put("()", b.val);

                                    var funcData = try self.allocator.create(Value);
                                    funcData.* = Value{
                                        .FunctionData = data,
                                    };

                                    return self.implCall(funcData, params, ctx);
                                },
                                else => {
                                    std.log.info("{s}", .{@tagName(b.kind.*)});
                                    return error.InvalidCall;
                                },
                            },
                            else => {
                                std.log.info("{s}", .{@tagName(func.*)});
                                return error.InvalidCall;
                            },
                        }
                    },
                    .ConstArray => {
                        for (eValues, oper.values) |*value, operValue| {
                            value.* = try self.visitExpr(operValue, ctx);
                        }

                        return .{
                            .ConstArray = eValues,
                        };
                    },
                    .Access => {
                        var root = try self.allocator.create(Value);
                        root.* = try self.visitExpr(values[0], ctx);
                        if (std.mem.eql(u8, values[1].data.Ident.name, "NAME")) {
                            return .{
                                .ConstString = root.getName() orelse "",
                            };
                        }

                        var name = values[1].data.Ident.name;

                        return try self.getProp(root, name);
                    },
                    .Assign => {
                        var assignee = try self.allocator.create(Value);
                        var value = try self.allocator.create(Value);
                        assignee.* = try self.visitExpr(values[0], ctx);
                        value.* = try self.visitExpr(values[1], ctx);

                        return try assignee.setTo(self, value);
                    },
                    .Ref => {
                        var value = try self.allocator.create(Value);
                        value.* = try self.visitExpr(values[0], ctx);

                        switch (value.*) {
                            .FunctionData => |data| {
                                var kind = try self.allocator.create(Value);
                                var val = try self.implFunc(value, null, false);

                                kind.* = .{
                                    .FunctionType = .{
                                        .in = data.kind.in,
                                        .out = data.kind.out,
                                        .kind = val.typeOf(),
                                        .fnkind = data.implkind.?,
                                    },
                                };

                                return .{
                                    .Value = .{
                                        .val = val,
                                        .kind = kind,
                                    },
                                };
                            },
                            else => return error.DumnDev,
                        }
                    },
                    .Deref => {
                        var value = try self.allocator.create(Value);
                        value.* = try self.visitExpr(values[0], ctx);
                        switch (value.*) {
                            .FunctionType, .PtrType, .Struct, .ArrayType, .IntType, .RealType => {
                                return .{
                                    .PtrType = .{
                                        .child = value,
                                    },
                                };
                            },
                            .Value => {
                                return value.toValue(self) orelse return error.InvalidValue;
                            },
                            .Builtin => |b| {
                                switch (b) {
                                    .VoidType => {
                                        return .{
                                            .PtrType = .{
                                                .child = value,
                                            },
                                        };
                                    },
                                    else => {
                                        return error.DumnDev;
                                    },
                                }
                            },
                            else => {
                                std.log.debug("{s}", .{@tagName(value.*)});
                                return error.DumnDev;
                            },
                        }
                    },
                    .IndexAccess => {
                        var value = try self.allocator.create(Value);
                        value.* = try self.visitExpr(values[0], ctx);

                        switch (value.*) {
                            .Struct, .IntType, .PtrType, .ArrayType => return .{
                                .ArrayType = .{
                                    .child = value,
                                    .size = if (values.len <= 1) null else (try self.visitExpr(values[1], ctx)).ConstInt,
                                },
                            },
                            .Value => |val| {
                                switch (val.kind.*) {
                                    .PtrType => |ptr| {
                                        switch (ptr.child.*) {
                                            .Struct => {
                                                var idx = try self.allocator.create(Value);
                                                idx.* = try self.visitExpr(values[1], ctx);

                                                var params = try self.allocator.alloc(Value, 2);
                                                params[0] = value.*;
                                                params[1] = idx.*;

                                                var tmp = ptr.child.*;

                                                var func = try self.allocator.create(Value);
                                                func.* = self.getProp(&tmp, "[]") catch |err| {
                                                    std.log.info("{} dosent have []", .{tmp});
                                                    return err;
                                                };

                                                return self.implCall(func, params, ctx);
                                            },
                                            .ArrayType => {
                                                var idx = try self.allocator.create(Value);
                                                idx.* = try self.visitExpr(values[1], ctx);

                                                const kind = try self.allocator.create(Value);
                                                kind.* = .{
                                                    .PtrType = .{
                                                        .child = ptr.child.ArrayType.child,
                                                    },
                                                };

                                                return .{
                                                    .Value = .{
                                                        .val = self.builder.buildInBoundsGEP(
                                                            ptr.child.ArrayType.child.getType(self) orelse return error.InvalidType,
                                                            value.getValue(self) orelse return error.InvalidValue,
                                                            &[_]*const llvm.Value{
                                                                idx.getValue(self) orelse return error.InvalidValue,
                                                            },
                                                            1,
                                                            defaultPrefix ++ "access",
                                                        ),
                                                        .kind = kind,
                                                    },
                                                };
                                            },
                                            else => {},
                                        }
                                    },
                                    else => {},
                                }
                            },
                            .ConstString => |_| {
                                var idx = try self.allocator.create(Value);
                                idx.* = try self.visitExpr(values[1], ctx);

                                const charKind = try self.allocator.create(Value);
                                charKind.* = .{
                                    .IntType = .{
                                        .size = 8,
                                        .signed = false,
                                    },
                                };

                                const kind = try self.allocator.create(Value);
                                kind.* = .{
                                    .PtrType = .{ .child = charKind },
                                };

                                return .{
                                    .Value = .{
                                        .val = self.builder.buildInBoundsGEP(
                                            self.context.intType(8),
                                            value.getValue(self) orelse return error.InvalidValue,
                                            &[_]*const llvm.Value{
                                                idx.getValue(self) orelse return error.InvalidValue,
                                            },
                                            1,
                                            defaultPrefix ++ "access",
                                        ),
                                        .kind = kind,
                                    },
                                };
                            },
                            .ConstArray => |arr| {
                                var idx = try self.allocator.create(Value);
                                idx.* = try self.visitExpr(values[1], ctx);

                                return arr[idx.ConstInt];
                            },
                            else => {},
                        }

                        std.log.info("{s}", .{value.*});

                        return error.DumnDev;
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
                if (std.mem.eql(u8, ident.name, "Type"))
                    return .{ .Builtin = .TypeType };
                if (std.mem.eql(u8, ident.name, "Void"))
                    return .{ .Builtin = .VoidType };
                if (std.mem.eql(u8, ident.name, "Class"))
                    return .{ .Builtin = .ClassType };

                if (std.mem.eql(u8, ident.name, "false"))
                    return .{ .Builtin = .FalseValue };
                if (std.mem.eql(u8, ident.name, "true"))
                    return .{ .Builtin = .TrueValue };

                if (std.mem.eql(u8, ident.name, "ERROR"))
                    return .{ .Builtin = .ErrorFn };

                if (std.mem.eql(u8, ident.name, "ADD_DEF"))
                    return .{ .Builtin = .AddDefFn };
                if (std.mem.eql(u8, ident.name, "GET_PROP"))
                    return .{ .Builtin = .GetPropFn };

                if (ident.name[0] == 'f') {
                    if (std.fmt.parseInt(c_uint, ident.name[1..], 0) catch null) |val| {
                        return .{
                            .RealType = val,
                        };
                    }
                }

                if (ident.name[0] == 'u') {
                    if (std.fmt.parseInt(c_uint, ident.name[1..], 0) catch null) |val| {
                        return .{
                            .IntType = .{
                                .size = val,
                                .signed = false,
                            },
                        };
                    }
                }

                if (ident.name[0] == 'i') {
                    if (std.fmt.parseInt(c_uint, ident.name[1..], 0) catch null) |val| {
                        return .{
                            .IntType = .{
                                .size = val,
                                .signed = true,
                            },
                        };
                    }
                }

                if (std.mem.eql(u8, ident.name, "usize")) {
                    return .{
                        .IntType = .{
                            .size = 64,
                            .signed = false,
                        },
                    };
                }

                if (std.mem.eql(u8, ident.name, "bool")) {
                    return .{
                        .IntType = .{
                            .size = 1,
                            .signed = false,
                        },
                    };
                }

                if (std.mem.eql(u8, ident.name, "null")) {
                    return .{
                        .Builtin = .NullValue,
                    };
                }

                if (std.mem.eql(u8, ident.name, "Self")) {
                    return ctx.current.Visited.value;
                }

                if (ctx.locals.get(ident.name)) |local| {
                    return local;
                }

                if (self.getNode(ctx.current, ident.name) catch null) |local| {
                    return (try self.visitNode(local, &ctx.current.Visited.value)).Visited.value;
                }

                return (try self.visitNode(try self.getNode(ctx.root, ident.name), &ctx.current.Visited.value)).Visited.value;
            },
            .ConstInt => |val| {
                return .{
                    .ConstInt = val.value,
                };
            },
            .ConstFloat => |val| {
                return .{
                    .ConstReal = val.value,
                };
            },
            .Proc => |proc| {
                if (proc.body) |body| {
                    var data = try self.allocator.create(FunctionDataType);

                    data.* = .{
                        .kind = .{
                            .in = try self.allocator.alloc(parser.Expression.FunctionInput, proc.in.len),
                            .out = try self.allocator.create(Value),
                        },
                        .context = try ctx.dupe(ctx.current),
                        .conts = body,
                        .impls = std.StringHashMap(*const llvm.Value).init(self.allocator),
                        .implkind = null,
                        .callKind = .Std,
                    };

                    var result = .{
                        .FunctionData = data,
                    };

                    for (proc.in, result.FunctionData.kind.in) |*expres, *res| {
                        res.* = expres.*;
                    }

                    result.FunctionData.kind.out.* = try self.visitExpr(proc.out.*, &result.FunctionData.context);

                    return result;
                }
                if (proc.ext) {
                    var data = try self.allocator.create(FunctionDataType);

                    data.* = .{
                        .kind = .{
                            .in = try self.allocator.alloc(parser.Expression.FunctionInput, proc.in.len),
                            .out = try self.allocator.create(Value),
                        },
                        .context = try ctx.dupe(ctx.current),
                        .conts = null,
                        .impls = std.StringHashMap(*const llvm.Value).init(self.allocator),
                        .implkind = null,
                        .callKind = .Ext,
                    };

                    var result = .{
                        .FunctionData = data,
                    };

                    for (proc.in, result.FunctionData.kind.in) |*expres, *res| {
                        res.* = expres.*;
                    }

                    result.FunctionData.kind.out.* = try self.visitExpr(proc.out.*, &result.FunctionData.context);

                    return result;
                }

                var result = Value{
                    .FunctionType = .{
                        .in = try self.allocator.alloc(parser.Expression.FunctionInput, proc.in.len),
                        .out = try self.allocator.create(Value),
                        .kind = @as(*const llvm.Type, undefined),
                        .fnkind = undefined,
                    },
                };

                for (proc.in, result.FunctionType.in) |*expres, *res| {
                    res.* = expres.*;
                }

                result.FunctionType.out.* = try self.visitExpr(proc.out.*, ctx);

                var argsTypes = try self.allocator.alloc(*const llvm.Type, result.FunctionType.in.len);
                for (argsTypes, result.FunctionType.in) |*arg, in| {
                    var tmp = try self.allocator.create(Value);
                    tmp.* = try self.visitExpr(in.kind.?.*, ctx);
                    arg.* = tmp.getType(self) orelse return error.InvalidType;
                }
                var functionType = llvm.functionType(result.FunctionType.out.getType(self) orelse return error.InvalidType, argsTypes.ptr, @intCast(argsTypes.len), .False);
                var function = self.module.addFunction("lol", functionType);

                result.FunctionType.kind = function.typeOf();
                result.FunctionType.fnkind = functionType;
                function.deleteFunction();

                return result;
            },
            .Paren => |data| {
                return self.visitExpr(data.expr.*, ctx);
            },
            .Struct => |cls| {
                var root = try self.allocator.create(InterpreterNode);
                var structData = try self.allocator.create(StructDataType);
                structData.* = .{
                    .values = std.StringHashMap(InterpreterNode).init(self.allocator),
                    .context = try ctx.dupe(root),
                    .parent = ctx.current,
                    .kind = self.context.voidType(),
                };

                root.* = .{
                    .Visited = .{
                        .name = defaultPrefix ++ "Struct",
                        .value = Value{
                            .Struct = structData,
                        },
                        .parent = ctx.current,
                    },
                };

                try root.Visited.value.Struct.context.locals.put("Self", root.Visited.value);

                var propIdx: usize = 0;
                var props = std.ArrayList(*const llvm.Type).init(self.allocator);

                for (cls.body) |*def| {
                    if (def.value.data != .Prop) {
                        try root.Visited.value.Struct.values.put(def.name, .{
                            .Unvisited = .{
                                .name = def.name,
                                .value = &def.value,
                                .ctx = try root.Visited.value.Struct.context.dupe(root),
                            },
                        });
                    }
                }

                for (cls.body) |*def| {
                    if (def.value.data == .Prop) {
                        var kind = try self.allocator.create(Value);
                        kind.* = try self.visitExpr(
                            def.value.data.Prop.kind.*,
                            &root.Visited.value.Struct.context,
                        );

                        try root.Visited.value.Struct.values.put(def.name, .{
                            .Visited = .{
                                .name = def.name,
                                .value = .{
                                    .Prop = .{
                                        .idx = propIdx,
                                        .kind = kind,
                                    },
                                },
                                .parent = root,
                            },
                        });

                        try props.append(kind.getType(self) orelse return error.InvalidType);

                        propIdx += 1;
                    }
                }

                root.Visited.value.Struct.kind = self.context.structType(props.items.ptr, @intCast(props.items.len), .False);

                return root.Visited.value;
            },
            .Create => |create| {
                if (ctx.function == null) {
                    var kind = try self.allocator.create(Value);
                    kind.* = try self.visitExpr(create.kind.*, ctx);

                    var pkind = try self.allocator.create(Value);
                    pkind.* = .{
                        .PtrType = .{
                            .child = kind,
                        },
                    };

                    var lKind = kind.getType(self) orelse return error.InvalidType;
                    const result = .{
                        .Value = .{
                            .val = self.module.addGlobal(lKind, defaultPrefix ++ "Global"),
                            .kind = pkind,
                        },
                    };

                    result.Value.val.setInitializer(lKind.constNull());

                    return result;
                }
                var kind = try self.allocator.create(Value);
                kind.* = try self.visitExpr(create.kind.*, ctx);
                var pkind = try self.allocator.create(Value);
                pkind.* = .{
                    .PtrType = .{
                        .child = kind,
                    },
                };

                var lKind = kind.getType(self) orelse return error.InvalidType;
                return .{
                    .Value = .{
                        .val = self.builder.buildAlloca(lKind, defaultPrefix ++ "Value"),
                        .kind = pkind,
                    },
                };
            },
            .Include => |f| {
                var file = try self.allocator.dupe(u8, f);
                if (!self.includes.contains(file)) {
                    var buff = try self.allocator.alloc(u8, 1000000);

                    var contsLen = (std.fs.cwd().openFile(file, .{}) catch return error.FailedImport).readAll(buff) catch return error.FailedImport;

                    var scn = scanner.Scanner.init(file, buff[0..contsLen]);
                    var psr = parser.Parser.init(scn, self.allocator);
                    var root = try self.allocator.dupe(parser.Definition, try psr.parse());

                    try self.includes.put(file, .{ .Bad = 0 });

                    const rootNode = self.includes.getPtr(file) orelse unreachable;
                    const structData = try self.allocator.create(StructDataType);
                    structData.* = .{
                        .values = std.StringHashMap(InterpreterNode).init(self.allocator),
                        .context = InterpreterContext{
                            .locals = std.StringHashMap(Value).init(self.allocator),
                            .current = rootNode,
                            .root = rootNode,
                        },
                        .parent = null,
                        .kind = undefined,
                    };
                    rootNode.* = .{
                        .Visited = .{
                            .name = file,
                            .parent = null,
                            .value = .{
                                .Struct = structData,
                            },
                        },
                    };

                    for (root) |*node| {
                        try rootNode.Visited.value.Struct.values.put(node.name, .{
                            .Unvisited = .{
                                .name = node.name,
                                .value = &node.value,
                                .ctx = try rootNode.Visited.value.Struct.context.dupe(rootNode),
                            },
                        });
                    }
                }

                return .{
                    .Include = .{
                        .name = file,
                    },
                };
            },
            .Prop => |node| {
                var kind = try self.allocator.create(Value);

                kind.* = try self.visitExpr(node.kind.*, ctx);

                return .{
                    .Prop = .{
                        .idx = null,
                        .kind = kind,
                    },
                };
            },
        }
    }

    pub fn visitNode(self: *Self, target: *InterpreterNode, parent: ?*Value) InterpreterError!*InterpreterNode {
        switch (target.*) {
            .Bad => return error.BadNode,
            .Visited => return target,
            .Visiting => {
                std.log.info("{s}", .{target.Visiting});
                return error.DoubleVisit;
            },
            .Unvisited => {
                var targ = target.Unvisited;

                target.* = .{
                    .Visiting = try self.allocator.dupe(u8, targ.name),
                };

                var result = try self.visitExpr(targ.value.*, &targ.ctx);

                try result.setName(targ.name);

                target.* = .{
                    .Visited = .{
                        .name = targ.name,
                        .value = result,
                        .parent = @fieldParentPtr(
                            InterpreterNode,
                            "Visited",
                            @fieldParentPtr(InterpreterNode.VisitedImpl, "value", parent.?),
                        ),
                    },
                };

                return target;
            },
        }
    }

    pub fn implStmt(self: *Self, stmt: *const parser.Statement, ctx: *InterpreterContext) InterpreterError!void {
        switch (stmt.data) {
            .Return => |ret| {
                if (ret) |val| {
                    var rets = (try self.visitExpr(val, ctx));
                    _ = self.builder.buildRet(rets.getValue(self) orelse return error.InvalidValue);
                } else {
                    _ = self.builder.buildRetVoid();
                }
            },
            .Definition => |def| {
                var val = try self.visitExpr(def.value, ctx);

                try val.setName(def.name);

                try ctx.locals.put(def.name, val);
            },
            .Expression => |expr| {
                _ = try self.visitExpr(expr, ctx);
            },
            .Block => |data| {
                for (data) |subStmt| {
                    try self.implStmt(&subStmt, ctx);
                }
            },
            .While => |data| {
                var headBB = self.context.appendBasicBlock(ctx.function.?, "whileHead");
                var bodyBB = self.context.appendBasicBlock(ctx.function.?, "whileBody");
                var mergeBB = self.context.appendBasicBlock(ctx.function.?, "whileMerge");

                _ = self.builder.buildBr(headBB);
                self.builder.positionBuilderAtEnd(headBB);

                var tmp = try self.visitExpr(data.check, ctx);
                var condV = tmp.getValue(self) orelse return error.InvalidValue;
                condV = self.builder.buildICmp(.NE, condV, condV.typeOf().constNull(), "whileCond");

                _ = self.builder.buildCondBr(condV, bodyBB, mergeBB);
                self.builder.positionBuilderAtEnd(bodyBB);

                try self.implStmt(data.body, ctx);

                _ = self.builder.buildBr(headBB);
                self.builder.positionBuilderAtEnd(mergeBB);
            },
            .If => |data| {
                var bodyBB = self.context.appendBasicBlock(ctx.function.?, "ifBody");
                var elseBB = self.context.appendBasicBlock(ctx.function.?, "ifElse");
                var mergeBB = self.context.appendBasicBlock(ctx.function.?, "ifMerge");

                var tmp = try self.visitExpr(data.check, ctx);
                var condV = tmp.getValue(self) orelse return error.InvalidValue;
                condV = self.builder.buildICmp(.NE, condV, condV.typeOf().constNull(), "ifCond");

                _ = self.builder.buildCondBr(condV, bodyBB, elseBB);
                self.builder.positionBuilderAtEnd(bodyBB);

                try self.implStmt(data.body, ctx);
                _ = self.builder.buildBr(mergeBB);

                self.builder.positionBuilderAtEnd(elseBB);
                if (data.bodyElse) |body| {
                    try self.implStmt(body, ctx);
                }
                _ = self.builder.buildBr(mergeBB);

                self.builder.positionBuilderAtEnd(mergeBB);
            },
            .For => |data| {
                var list = try self.visitExpr(data.list, ctx);
                switch (list) {
                    .ConstArray => |l| {
                        var sctx = try ctx.dupe(ctx.current);
                        for (0..l.len) |idx| {
                            try sctx.locals.put(data.vName, l[idx]);

                            try self.implStmt(data.body, &sctx);
                        }
                    },
                    //.Value => |l| {
                    //    var sctx = try ctx.dupe(ctx.current);
                    //    for (0..l.len) |idx| {
                    //        try sctx.locals.put(data.vName, l[idx]);

                    //        for (data.body) |subStmt| {
                    //            try self.implStmt(&subStmt, &sctx);
                    //        }
                    //    }
                    //},
                    else => {
                        std.debug.print("{}\n", .{data});
                        return error.DumnDev;
                    },
                }
            },
            //else => |data| {
            //    std.debug.print("{}\n", .{data});
            //    return error.DumnDev;
            //},
        }
    }

    pub fn implFunc(self: *Self, target: *Value, params: ?[]Value, forceDef: bool) InterpreterError!*const llvm.Value {
        switch (target.*) {
            .FunctionData => |func| {
                var impl_name: []const u8 = try self.allocator.dupe(u8, "");

                var name = try self.allocator.dupe(u8, "(");

                if (params) |ps| {
                    for (ps) |*param| {
                        if (param.* != .Struct) continue;

                        const old = name;
                        defer self.allocator.free(old);
                        name = try std.mem.concat(self.allocator, u8, &.{ old, param.getName().?, "," });
                    }

                    const old = name;
                    defer self.allocator.free(old);
                    const start = if (name.len == 1) old else old[0 .. old.len - 1];

                    name = try std.mem.concat(self.allocator, u8, &.{ start, ")" });
                } else {
                    const old = name;
                    defer self.allocator.free(old);
                    name = try std.mem.concat(self.allocator, u8, &.{ old, ")" });
                }

                if (func.impls.get(name)) |impl| return impl;

                impl_name = name;

                var ctx = try func.context.dupe(func.context.current);

                var out = func.kind.out.getType(self) orelse return error.InvalidType;
                var argsTypes = try self.allocator.alloc(*const llvm.Type, func.kind.in.len);
                var argsTypesValues = try self.allocator.alloc(Value, func.kind.in.len);
                for (argsTypesValues, argsTypes, func.kind.in) |*argValue, *arg, in| {
                    argValue.* = try self.visitExpr(in.kind.?.*, &ctx);
                    arg.* = argValue.getType(self) orelse return error.InvalidType;
                }
                var functionType = llvm.functionType(out, argsTypes.ptr, @intCast(argsTypes.len), .False);

                func.implkind = functionType;

                var newName = try std.mem.concat(self.allocator, u8, &.{ func.name, if (forceDef) "" else impl_name, "\x00" });
                defer self.allocator.free(newName);

                var function = self.module.addFunction(@ptrCast(newName.ptr), functionType);

                for (func.kind.in, 0..) |arg, idx| {
                    if (argsTypesValues[idx] == .Builtin) {
                        try ctx.locals.put(arg.name, params.?[idx]);
                    } else {
                        try ctx.locals.put(arg.name, .{
                            .Value = .{
                                .val = function.getParam(@intCast(idx)),
                                .kind = &argsTypesValues[idx],
                            },
                        });
                    }
                }

                var oldBB = self.builder.getInsertBlock();
                var bb = self.context.appendBasicBlock(function, "entry");
                self.builder.positionBuilderAtEnd(bb);

                ctx.function = function;
                try func.impls.put(impl_name, function);

                if (func.conts) |conts| {
                    try self.implStmt(conts, &ctx);
                }

                self.builder.positionBuilderAtEnd(oldBB);

                //function.setFunctionName(try std.mem.concat(self.allocator, u8, &.{ func.name, impl_name }));

                return function;
            },
            else => {
                std.debug.print("{}\n", .{target});
                return error.DumnDev;
            },
        }
    }

    pub fn implNode(self: *Self, target: *InterpreterNode, parent: ?*Value) InterpreterError!*const llvm.Value {
        switch (target.*) {
            .Bad => return error.BadNode,
            .Visited => {
                return self.implFunc(&target.Visited.value, null, false);
            },
            .Visiting => {
                std.log.info("{s}", .{target.Visiting});

                return error.DoubleVisit;
            },
            .Unvisited => return try self.implNode(try self.visitNode(target, parent), parent),
        }
    }
};
