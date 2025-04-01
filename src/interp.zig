const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");

pub const TokenIndex = token.TokenIndex;
pub const Token = token.Token;
pub const NodeIndex = ast.NodeIndex;
pub const Ast = ast.Ast;

pub const Interp = struct {
    gpa: std.mem.Allocator,
    ast: Ast,
    bound: std.StringHashMap(Value),

    const Enviornment = struct {
        bound: std.StringHashMap(Value),
        parent: *Enviornment,
    };

    pub const ValueData = union(enum) {
        int: i32,
        float: f64,
        bool: bool,
        sym: []const u8,
        str: []const u8,
        // todo figure out whether lst is a function as well
        lst: []const Value,
        bif: *const fn (*Interp, args: []Value) Value,
        bim: *const fn (*Interp, args: []Value) Value,
    };

    pub const Value = struct {
        data: ValueData,
        history: *const HistoryNode,

        // should nil values have history? probably, but for now..
        pub const nil: Value = .{ .data = .{ .lst = &.{} }, .history = undefined };
    };

    pub fn init(allocator: std.mem.Allocator, ast_tree: Ast) !Interp {
        var interp: Interp = .{
            .gpa = allocator,
            .ast = ast_tree,
            .bound = std.StringHashMap(Interp.Value).init(allocator),
        };

        _ = try interp.bound.put("true", .{ .data = .{ .bool = true }, .history = &.builtin });
        _ = try interp.bound.put("false", .{ .data = .{ .bool = false }, .history = &.builtin });

        return interp;
    }

    pub fn inspect(i: Interp, value: Value) void {
        return i.inspectOptions(value, 0);
    }

    fn inspectOptions(i: Interp, value: Value, tabs: u32) void {
        const tab_str = i.gpa.alloc(u8, tabs) catch @panic("oom");
        defer i.gpa.free(tab_str);
        @memset(tab_str, '\t');
        std.debug.print("{s}value: {any}\n", .{ tab_str, value.data });
        var next: ?*const HistoryNode = value.history;
        while (next) |hist| {
            switch (hist.data) {
                .creation => |c| std.debug.print("{s}created @ character {d}\n", .{ tab_str, c }),
                .call => |c| {
                    std.debug.print("{s}call \"{s}\" @ character {d} with {d} arguments:\n", .{
                        tab_str,
                        c.fn_name,
                        i.ast.tokens[i.ast.nodes[c.node].main_token].loc.start,
                        c.args.len,
                    });

                    for (c.args) |arg| {
                        i.inspectOptions(arg, tabs + 1);
                    }
                },
                .builtin => std.debug.print("{s}built in", .{tab_str}),
                .macro => std.debug.print("{s}constructed at macro", .{tab_str}),
            }
            next = hist.next;
        }
    }

    pub const HistoryNode = struct {
        next: ?*const HistoryNode,
        data: union(enum) {
            // creation line number (for now it points to source index)
            creation: usize,
            // points to the expr call
            call: struct {
                node: NodeIndex,
                fn_name: []const u8,
                args: []const Value,
            },
            builtin: void,
            macro: void,
        },
        pub const builtin: HistoryNode = .{ .next = null, .data = .builtin };
    };

    pub fn evalRoot(i: *Interp) !void {
        // Process each expression in the root node
        for (i.ast.root_node.data.root) |expr| {
            const expr_value = try i.nodeToValue(expr);
            _ = try i.eval(expr_value);
        }
    }

    fn creationNode(i: *Interp, node_i: NodeIndex) !*const HistoryNode {
        const hist = try i.gpa.create(HistoryNode);
        const node = i.ast.nodes[node_i];
        hist.* = .{
            .next = null,
            .data = .{ .creation = i.ast.tokens[node.main_token].loc.start },
        };
        return hist;
    }

    pub fn GetValueDataFieldType(comptime field_name: []const u8) type {
        for (@typeInfo(ValueData).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) return field.type;
        }
        @compileError("invalid field name: " ++ field_name);
    }

    pub fn createValueNoHistory(comptime field_name: []const u8, data: GetValueDataFieldType(field_name)) Value {
        return .{
            .data = @unionInit(ValueData, field_name, data),
            .history = undefined,
        };
    }

    pub fn createValue(i: *Interp, comptime field_name: []const u8, data: GetValueDataFieldType(field_name), node: NodeIndex) Value {
        return .{
            .data = @unionInit(ValueData, field_name, data),
            .history = i.creationNode(node) catch @panic("oom"),
        };
    }

    // Modified nodeToValue to not call eval
    pub fn nodeToValue(i: *Interp, node: NodeIndex) !Value {
        const atom = i.ast.nodes[node];

        return val: switch (atom.data) {
            .int => |num| i.createValue("int", num, node),
            .float => |num| i.createValue("float", num, node),
            .str => |str| i.createValue("str", str, node),
            .sym => |sym| i.createValue("sym", sym, node),
            .expr => |expr| {
                var values = try i.gpa.alloc(Value, expr.len);
                for (expr, 0..) |child, child_i| {
                    values[child_i] = try i.nodeToValue(child);
                }

                break :val i.createValue("lst", values, node);
            },
            .root => unreachable,
        };
    }

    // Modified eval to take a Value instead of a node index
    pub fn eval(i: *Interp, value: Value) std.mem.Allocator.Error!Value {
        return val: switch (value.data) {
            .int, .float, .str => value,
            .sym => |sym| i.bound.get(sym) orelse @panic("unbound variable"),
            .lst => |lst| {
                if (lst.len == 0) return value;

                const first = lst[0];
                switch (first.data) {
                    .sym => |fn_name| {
                        // TODO instead of hard coding this make it a bif_macro
                        const symbol = i.bound.get(fn_name) orelse @panic("tried to call unbound variable");

                        switch (symbol.data) {
                            .bif => |proc| {
                                var args = try i.gpa.alloc(Value, lst.len - 1);
                                for (lst[1..], 0..) |arg, vi| {
                                    args[vi] = try i.eval(arg);
                                }

                                var res = proc(i, args);
                                const hist = try i.gpa.create(HistoryNode);
                                hist.* = .{
                                    .next = null,
                                    .data = .{
                                        .call = .{
                                            .node = 0, // We don't have a node reference anymore
                                            .fn_name = fn_name,
                                            .args = args,
                                        },
                                    },
                                };

                                res.history = hist;
                                break :val res;
                            },
                            .bim => |proc| {
                                // TODO add macro expansion as a history node type
                                var args = try i.gpa.alloc(Value, lst.len - 1);
                                for (lst[1..], 0..) |arg, vi| {
                                    args[vi] = arg;
                                }

                                var res = proc(i, args);
                                const hist = try i.gpa.create(HistoryNode);
                                hist.* = .{ .next = null, .data = .macro };
                                res.history = hist;
                                break :val res;
                            },
                            else => @panic("i dunno how to call this."),
                        }
                    },
                    else => @panic("can not call non-symbol"),
                }
                break :val i.createValueNoHistory(
                    "str",
                    "THIS SHOULD NEVER BE REACHED (BUT IT HAS???)",
                );
            },
            else => @panic("cannot evaluate this value type"),
        };
    }
};
