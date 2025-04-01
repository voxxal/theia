const std = @import("std");

pub const TokenIndex = u32;
pub const NodeIndex = u32;

const Token = struct {
    const Tag = enum {
        invalid,
        lp,
        rp,
        quote,
        int,
        float,
        sym,
        str,
        eof,
    };

    const Loc = struct {
        start: usize,
        end: usize,
    };
    tag: Tag,
    loc: Loc,
};

const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    const State = enum {
        invalid,
        start,
        sym,
        int,
        float,
        str,
        str_escape,
    };

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present.
        return .{
            .buffer = buffer,
            .index = 0,
        };
    }

    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        state: switch (State.start) {
            // TODO fix this :)
            .invalid => unreachable,
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                '(' => {
                    result.tag = .lp;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .rp;
                    self.index += 1;
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                'a'...'z', 'A'...'Z', '_', '+', '-', '*', '/', '!', '<'...'>' => {
                    result.tag = .sym;
                    continue :state .sym;
                },
                '"' => {
                    result.tag = .str;
                    continue :state .str;
                },
                '\'' => {
                    result.tag = .quote;
                    self.index += 1;
                    continue :state .start;
                },
                // TODO add support for negative number
                '0'...'9' => {
                    result.tag = .int;
                    continue :state .int;
                },
                else => continue :state .invalid,
            },

            .str => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => continue :state .str_escape,
                    '"' => self.index += 1,
                    0x01...0x09, 0x0b...0x1f, 0x7f => {
                        continue :state .invalid;
                    },
                    else => continue :state .str,
                }
            },

            .str_escape => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .str,
                }
            },

            .sym => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9', '+', '-', '*', '/', '!', '<'...'>' => continue :state .sym,
                    else => {},
                }
            },

            .int => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => continue :state .int,
                    '.' => continue :state .float,
                    else => {},
                }
            },

            .float => {
                self.index += 1;
                if (result.tag == .float) @panic("lex fail, found 2 . in float");
                result.tag = .float;
                continue :state .int;
            },
        }

        result.loc.end = self.index;
        return result;
    }
};

const Parse = struct {
    source: []const u8,
    gpa: std.mem.Allocator,
    tok_i: TokenIndex,
    tokens: std.ArrayList(Token),
    nodes: std.ArrayListUnmanaged(Ast.Node),
    errors: std.ArrayListUnmanaged(Error),

    const Error = struct {
        tag: Tag,
        token_index: TokenIndex,

        const Tag = enum {
            unexpected_token,
        };
    };

    fn currToken(p: *Parse) Token {
        return p.token(p.tok_i);
    }

    fn token(p: *Parse, i: TokenIndex) Token {
        return p.tokens.items[i];
    }

    fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
        return if (p.currToken().tag == tag) p.nextToken() else null;
    }

    fn expectToken(p: *Parse, tag: Token.Tag) !TokenIndex {
        if (p.currToken().tag != tag) {
            try p.errors.append(p.gpa, .{
                .tag = .unexpected_token,
                .token_index = p.tok_i,
            });
            return error.ParseError;
        }
        return p.nextToken();
    }

    fn nextToken(p: *Parse) TokenIndex {
        const result = p.tok_i;
        p.tok_i += 1;
        return result;
    }

    fn addNode(p: *Parse, elem: Ast.Node) !NodeIndex {
        const result: NodeIndex = @intCast(p.nodes.items.len);
        try p.nodes.append(p.gpa, elem);
        return result;
    }

    fn parseRoot(p: *Parse) !Ast.Node {
        var nodes: std.ArrayList(NodeIndex) = .init(p.gpa);
        defer nodes.deinit();

        while (p.currToken().tag != .eof) {
            try nodes.append(try p.parseExpr());
        }
        const root: Ast.Node = .{
            .main_token = 0,
            .data = .{ .root = try nodes.toOwnedSlice() },
        };
        _ = try p.addNode(root);
        return root;
    }

    fn addQuoteNode(p: *Parse, quote_index: TokenIndex) !NodeIndex {
        return try p.addNode(.{
            .main_token = quote_index,
            .data = .{ .sym = "quote" },
        });
    }

    fn parseExpr(p: *Parse) !NodeIndex {
        const i = p.nextToken();
        const tok = p.token(i);
        const text = p.source[tok.loc.start..tok.loc.end];

        const node: Ast.Node = nd: switch (tok.tag) {
            .int, .float, .str, .sym, .quote => Ast.Node{
                .main_token = i,
                .data = switch (tok.tag) {
                    .int => .{ .int = try std.fmt.parseInt(i32, text, 10) },
                    .float => .{ .float = try std.fmt.parseFloat(f64, text) },
                    .str => .{ .str = text },
                    .sym => .{ .sym = text },
                    .quote => .{ .expr = &.{ try p.addQuoteNode(i), try p.parseExpr() } },
                    else => unreachable,
                },
            },
            .lp => {
                var nodes: std.ArrayList(NodeIndex) = .init(p.gpa);
                defer nodes.deinit();
                const main_token = p.tok_i;

                while (p.eatToken(.rp) == null) {
                    try nodes.append(try p.parseExpr());
                }

                const node: Ast.Node = .{
                    .main_token = main_token,
                    .data = .{ .expr = try nodes.toOwnedSlice() },
                };

                break :nd node;
            },
            // TODO throw an error onto the struct instead
            else => unreachable,
        };
        return try p.addNode(node);
    }
};

const Ast = struct {
    gpa: std.mem.Allocator,
    source: [:0]const u8,
    nodes: []const Node,
    tokens: []const Token,
    root_node: Node,
    errors: []const Parse.Error,

    const NodeData = union(enum) {
        root: []const NodeIndex,
        expr: []const NodeIndex,
        int: i32,
        float: f64,
        sym: []const u8,
        str: []const u8,
    };

    const Node = struct {
        main_token: TokenIndex,
        data: NodeData,
    };

    pub fn parse(gpa: std.mem.Allocator, source: [:0]const u8) !Ast {
        var tokens: std.ArrayList(Token) = .init(gpa);

        var tokenizer = Tokenizer.init(source);
        while (true) {
            const token = tokenizer.next();
            try tokens.append(token);
            if (token.tag == .eof) break;
        }

        var parser: Parse = .{
            .gpa = gpa,
            .source = source,
            .tok_i = 0,
            .tokens = tokens,
            .nodes = .{},
            .errors = .{},
        };
        defer parser.errors.deinit(gpa);
        defer parser.nodes.deinit(gpa);

        const root_node = try parser.parseRoot();

        return .{
            .gpa = gpa,
            .source = source,
            .nodes = try parser.nodes.toOwnedSlice(gpa),
            .tokens = try parser.tokens.toOwnedSlice(),
            .root_node = root_node,
            .errors = try parser.errors.toOwnedSlice(gpa),
        };
    }
};

const Interp = struct {
    gpa: std.mem.Allocator,
    ast: Ast,
    bound: std.StringHashMap(Value),

    const Enviornment = struct {
        bound: std.StringHashMap(Value),
        parent: *Enviornment,
    };

    const ValueData = union(enum) {
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

    const Value = struct {
        data: ValueData,
        history: *const HistoryNode,

        // should nil values have history? probably, but for now..
        const nil: Value = .{ .data = .{ .lst = &.{} }, .history = undefined };
    };

    fn init(allocator: std.mem.Allocator, ast: Ast) !Interp {
        var interp: Interp = .{
            .gpa = allocator,
            .ast = ast,
            .bound = std.StringHashMap(Interp.Value).init(allocator),
        };

        _ = try interp.bound.put("true", .{ .data = .{ .bool = true }, .history = &.builtin });
        _ = try interp.bound.put("false", .{ .data = .{ .bool = false }, .history = &.builtin });

        return interp;
    }
    fn inspect(i: Interp, value: Value) void {
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

    const HistoryNode = struct {
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
        const builtin: HistoryNode = .{ .next = null, .data = .builtin };
    };

    fn evalRoot(i: *Interp) !void {
        for (i.ast.root_node.data.root) |expr| {
            _ = try i.eval(expr);
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

    fn GetValueDataFieldType(comptime field_name: []const u8) type {
        for (@typeInfo(ValueData).@"union".fields) |field| {
            if (std.mem.eql(u8, field.name, field_name)) return field.type;
        }
        @compileError("invalid field name: " ++ field_name);
    }

    fn createValueNoHistory(comptime field_name: []const u8, data: GetValueDataFieldType(field_name)) Value {
        return .{
            .data = @unionInit(ValueData, field_name, data),
            .history = undefined,
        };
    }

    fn createValue(i: *Interp, comptime field_name: []const u8, data: GetValueDataFieldType(field_name), node: NodeIndex) Value {
        return .{
            .data = @unionInit(ValueData, field_name, data),
            .history = i.creationNode(node) catch @panic("oom"),
        };
    }

    fn nodeToValue(i: *Interp, node: NodeIndex) !Value {
        const atom = i.ast.nodes[node];

        return val: switch (atom.data) {
            .int, .float, .str => try i.eval(node),
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

    // TODO rework this to the sig `fn eval(i: *Interp, value: Value) ...
    // since in lisp, values are syntax!
    fn eval(i: *Interp, node: NodeIndex) std.mem.Allocator.Error!Value {
        return val: switch (i.ast.nodes[node].data) {
            .int => |num| i.createValue("int", num, node),
            .float => |num| i.createValue("float", num, node),
            .str => |str| i.createValue("str", str, node),
            .sym => |sym| i.bound.get(sym) orelse @panic("unbound variable"),
            .expr => |expr| {
                if (expr.len == 0) break :val i.createValue("lst", &.{}, node);
                switch (i.ast.nodes[expr[0]].data) {
                    .sym => |fn_name| {
                        // TODO instead of hard coding this make it a bif_macro
                        const symbol = i.bound.get(fn_name) orelse @panic("tried to call unbound variable");

                        switch (symbol.data) {
                            .bif => |proc| {
                                var args = try i.gpa.alloc(Value, expr.len - 1);
                                for (expr[1..], 0..) |atom_id, vi| {
                                    args[vi] = try i.eval(atom_id);
                                }
                                // TODO if the number of arguments is 1 or we see a rebind
                                // attach the history to the value
                                // otherwise just create a new value
                                var res = proc(i, args);
                                const hist = try i.gpa.create(HistoryNode);
                                hist.* = .{
                                    .next = null,
                                    .data = .{
                                        .call = .{
                                            .node = node,
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
                                var args = try i.gpa.alloc(Value, expr.len - 1);
                                for (expr[1..], 0..) |atom_id, vi| {
                                    args[vi] = try i.nodeToValue(atom_id);
                                }

                                var res = proc(i, args);
                                const hist = try i.gpa.create(HistoryNode);
                                hist.* = .{ .next = null, .data = .macro };
                                res.history = hist;
                                // TODO i think i need to evaluate this, but also it relies on chunk indexes which causes problems...
                            },
                            else => @panic("i dunno how to call this."),
                        }
                    },

                    else => {
                        // error!
                        @panic("can not call non-symbol");
                    },
                }
                break :val i.createValue("str", "THIS SHOULD NEVER BE REACHED (BUT IT HAS???)", node);
                // unreachable; // i think
            },
            // .str => |str| break :val createValue("str", node, str),
            // .expr => sob,

            .root => unreachable,
        };
    }
};

const stdout = std.io.getStdOut();
const out_writer = stdout.writer();

const TheiaBuiltin = struct {
    const Binding = struct {
        []const u8,
        *const fn (*Interp, args: []Interp.Value) Interp.Value,
    };
    const bifs: []const Binding = &[_]Binding{
        .{ "print", &print },
        .{ "+", &add },
    };

    const bims: []const Binding = &[_]Binding{
        .{ "quote", &quote },
        .{ "if", &ifs },
    };

    fn attach(i: *Interp) !void {
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
    fn quote(i: *Interp, args: []Interp.Value) Interp.Value {
        std.debug.assert(args.len == 1);
        _ = i;

        return args[0];
    }

    fn ifs(i: *Interp, args: []Interp.Value) Interp.Value {
        _ = i;
        std.debug.assert(args.len == 2 or args.len == 3);
        return switch (args[0].data) {
            // .bool => |b| if (b) i.eval(args[1]) catch @panic("oom") else if (args.len == 3) i.eval(args[2]) catch @panic("oom") else .nil,
            else => @panic("condition not boolean"),
        };
    }

    // functions

    fn print(i: *Interp, args: []Interp.Value) Interp.Value {
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

    fn add(i: *Interp, args: []Interp.Value) Interp.Value {
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

    const ast = try Ast.parse(allocator, source);
    for (ast.nodes, 0..) |node, i| {
        switch (node.data) {
            inline else => |v| std.debug.print("{d} - {s} node {any}\n", .{ i, @tagName(node.data), v }),
        }
        // switch (node.data) {
        //     .int => |d| ,
        //     .float => |f| std.debug.print("{d} - float node {d}", .{ i, f }),
        //     .bool => |b| std.debug.print("{d} - bool node {any}", .{ i, b }),
        //     .expr => |e| std.debug.print("{d} - expr node {any}\n", .{ i, e }),
        //     .root => |r| std.debug.print("{d} - root node {any}\n", .{ i, r }),
        //     .str => |s| std.debug.print("{d} - str node {s}\n", .{ i, s }),
        //     .sym => |s| std.debug.print("{d} - sym node {s}\n", .{ i, s }),
        // }
    }

    var interp: Interp = try .init(allocator, ast);

    try TheiaBuiltin.attach(&interp);
    try interp.evalRoot();
}
