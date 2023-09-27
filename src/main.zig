const std = @import("std");
const scanner = @import("lib/scanner.zig");
const parser = @import("lib/parser.zig");
const interpreter = @import("lib/interpreter.zig");
const llvm = @import("lib/llvm.zig");

pub fn main() !void {
    //var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    //const allocator = gpa.allocator();
    const allocator = std.heap.c_allocator;

    var args = try std.process.ArgIterator.initWithAllocator(allocator);

    _ = args.next();

    var inputFile: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (inputFile == null) {
            inputFile = arg;
        } else {
            return error.ExtraArg;
        }
    }

    if (inputFile == null) return error.NoFile;

    var buff = try allocator.alloc(u8, 10000);
    var contsLen = try (try std.fs.cwd().openFile(inputFile.?, .{})).readAll(buff);

    var scn = scanner.Scanner.init(inputFile.?, buff[0..contsLen]);
    var psr = parser.Parser.init(scn, allocator);
    var root = try psr.parse();

    //for (root) |node|
    //    std.debug.print("{}\n", .{node});

    var int = try interpreter.Interpreter.init(root, allocator);

    var tmp = [_]u8{0} ** 1000;

    int.module.setTarget("x86_64");

    _ = int.module.printModuleToFile("ir", @ptrCast(@alignCast(&tmp)));

    std.debug.print("LLVM lol.o\n", .{});

    const CPU: [*:0]const u8 = "x86-64";
    const features: [*:0]const u8 = "";
    const thriple: [*:0]const u8 = "x86_64";
    const out: [*:0]const u8 = "lol.o";
    var opt: ?*llvm.RelocMode = null;
    var t: *llvm.Target = undefined;
    var err: [*:0]const u8 = @as([*:0]const u8, @ptrCast(try allocator.alloc(u8, 512)));
    if (llvm.Target.getFromTriple(thriple, &t, &err).toBool()) {
        std.log.info("{s}", .{err});
    }

    var targetMachine = llvm.TargetMachine.create(t, thriple, CPU, features, opt, .Aggressive);

    targetMachine.emitToFile(int.module, out, .ObjectFile);

    std.debug.print("CC a.out\n", .{});

    var output = try std.ChildProcess.exec(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "gcc", "lol.o", "-L/usr/lib", "-lSDL2", "-I/usr/include/SDL2", "-D_REENTRANT", "-lSDL2_image" },
    });

    if (output.stdout.len != 0)
        std.log.info("{s}", .{output.stdout});
    if (output.stderr.len != 0)
        std.log.err("{s}", .{output.stderr});
}
