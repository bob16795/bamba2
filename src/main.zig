const std = @import("std");
const scanner = @import("lib/scanner.zig");
const parser = @import("lib/parser.zig");
const interpreter = @import("lib/interpreter.zig");
const llvm = @import("lib/llvm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

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

    var buff = try allocator.alloc(u8, 1000000);
    var contsLen = try (try std.fs.cwd().openFile(inputFile.?, .{})).readAll(buff);

    var scn = scanner.Scanner.init(inputFile.?, buff[0..contsLen]);
    var psr = parser.Parser.init(scn, allocator);
    var root = try psr.parse();

    var int = try interpreter.Interpreter.init(root, allocator);

    var mainNode = (try int.visitNode(try int.getNode(null, "main"), null)).*;
    try int.implNode(&mainNode, null);

    var str = int.module.printToString();

    var file = try std.fs.cwd().createFile("ir", .{});

    var writing: []const u8 = undefined;
    writing.ptr = @ptrCast([*]const u8, str);
    writing.len = 1;
    while (writing[writing.len - 1] != 0) writing.len += 1;
    writing.len -= 1;

    _ = try file.write(writing);
}
