const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const interp = @import("interp.zig");
const builtins = @import("builtins.zig");

const Tokenizer = token.Tokenizer;
const Ast = ast.Ast;
const Interp = interp.Interp;
const TheiaBuiltin = builtins.TheiaBuiltin;

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    const allocator = gpa.allocator();
    var args = std.process.args();
    const bin_name = args.next() orelse unreachable;

    const file_path = args.next() orelse {
        std.debug.print("usage: {s} FILE", .{bin_name});
        std.process.exit(1);
    };

    const source = try std.fs.cwd().readFileAllocOptions(
        allocator,
        file_path,
        std.math.maxInt(u32),
        null,
        @alignOf(u8),
        0,
    );

    const ast_tree = try Ast.parse(allocator, source);
    for (ast_tree.nodes, 0..) |node, i| {
        switch (node.data) {
            inline else => |v| std.debug.print("{d} - {s} node {any}\n", .{ i, @tagName(node.data), v }),
        }
    }

    var interpreter: Interp = try Interp.init(allocator, ast_tree);

    try TheiaBuiltin.attach(&interpreter);
    try interpreter.evalRoot();
}

// Re-export important types for external use
pub const TokenIndex = token.TokenIndex;
pub const Token = token.Token;
pub const NodeIndex = ast.NodeIndex;
