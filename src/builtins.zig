const std = @import("std");
const interp = @import("interp.zig");

const Interp = interp.Interp;
const Value = Interp.Value;

const stdout = std.io.getStdOut();
const out_writer = stdout.writer();

pub const TheiaBuiltin = struct {
    pub const Binding = struct {
        []const u8,
        *const fn (*Interp, args: []Value) Value,
    };

    pub const bifs: []const Binding = &[_]Binding{
        .{ "print", &print },
        .{ "+", &add },
    };

    pub const bims: []const Binding = &[_]Binding{
        .{ "quote", &quote },
        .{ "if", &ifs },
    };

    pub fn attach(i: *Interp) !void {
        for (bifs) |tup| {
            const name, const fun = tup;
            try i.bound.put(name, .{
                .data = .{ .bif = fun },
                .history = &.builtin,
            });
        }

        for (bims) |tup| {
            const name, const fun = tup;
            try i.bound.put(name, .{
                .data = .{ .bim = fun },
                .history = &.builtin,
            });
        }
    }

    // macros
    fn quote(i: *Interp, args: []Value) Value {
        std.debug.assert(args.len == 1);
        _ = i;

        return args[0];
    }

    fn ifs(i: *Interp, args: []Value) Value {
        _ = i;
        std.debug.assert(args.len == 2 or args.len == 3);
        return switch (args[0].data) {
            // .bool => |b| if (b) i.eval(args[1]) catch @panic("oom") else if (args.len == 3) i.eval(args[2]) catch @panic("oom") else .nil,
            else => @panic("condition not boolean"),
        };
    }

    // functions
    fn print(i: *Interp, args: []Value) Value {
        std.debug.assert(args.len == 1);
        i.inspect(args[0]);
        switch (args[0].data) {
            .str => |str| out_writer.print("{s}\n", .{str}) catch @panic("write failed"),
            .int => |int| out_writer.print("{d}\n", .{int}) catch @panic("write failed"),
            .float => |float| out_writer.print("{d}\n", .{float}) catch @panic("write failed"),
            else => @panic("unimplemented"),
        }

        return .nil;
    }

    fn add(i: *Interp, args: []Value) Value {
        _ = i;
        switch (args[0].data) {
            .str => {
                // str concat
                return .nil;
            },
            .int => {
                var res: i32 = 0;
                for (args) |arg| {
                    res += arg.data.int;
                }
                return Interp.createValueNoHistory("int", res);
            },
            else => @panic("unimplemented"),
        }
    }
};
