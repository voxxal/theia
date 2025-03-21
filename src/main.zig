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
                'a'...'z', 'A'...'Z', '_', '+', '-', '*', '/' => {
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
                    'a'...'z', 'A'...'Z', '_', '0'...'9', '+', '-', '*', '/' => continue :state .sym,
                    else => {},
                }
            },

            .int => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => continue :state .int,
                    else => {},
                }
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
            .int, .str, .sym, .quote => Ast.Node{
                .main_token = i,
                .data = switch (tok.tag) {
                    .int => .{ .int = try std.fmt.parseInt(i32, text, 10) },
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

    const ValueData = union(enum) {
        int: i32,
        sym: []const u8,
        str: []const u8,
        // todo figure out whether lst is a function as well
        lst: []const Value,
        bif: *const fn (*Interp, args: []Value) Value,
    };
    const Value = struct {
        data: ValueData,
        history: *HistoryNode,

        // should nil values have history? probably, but for now..
        const nil: Value = .{ .data = .{ .lst = &.{} }, .history = undefined };
    };

    const HistoryNode = struct {
        next: ?*HistoryNode,
        data: union(enum) {
            // creation line number (for now it points to source index)
            creation: usize,
            // points to the expr call
            call: struct {
                node: NodeIndex,
                fn_name: []const u8,
                args: []const Value,
            },
        },
    };

    fn evalRoot(i: *Interp) !void {
        for (i.ast.root_node.data.root) |expr| {
            _ = try i.eval(i.ast.nodes[expr]);
        }
    }

    fn creationNode(i: *Interp, node: Ast.Node) !*HistoryNode {
        const hist = try i.gpa.create(HistoryNode);
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

    fn createValue(i: *Interp, comptime field_name: []const u8, data: GetValueDataFieldType(field_name), node: Ast.Node) Value {
        return .{
            .data = @unionInit(ValueData, field_name, data),
            .history = i.creationNode(node) catch @panic("oom"),
        };
    }

    // TODO switch to NodeIndexes because they actually make code cleaner (no use of index of)
    fn eval(i: *Interp, node: Ast.Node) !Value {
        return val: switch (node.data) {
            .int => |num| break :val i.createValue("int", num, node),
            .str => |str| break :val i.createValue("str", str, node),
            .sym => |sym| break :val i.bound.get(sym) orelse @panic("unbound variable"),
            .expr => |expr| {
                if (expr.len == 0) break :val i.createValue("lst", &.{}, node);
                switch (i.ast.nodes[expr[0]].data) {
                    .sym => |fn_name| {
                        if (std.mem.eql(u8, fn_name, "quote")) {
                            if (expr.len != 2) @panic("quote called with wrong number of arguments");
                            const quoted = i.ast.nodes[expr[1]];
                            var values = try i.gpa.alloc(Value, quoted.data.expr.len);
                            // todo make sure is expr
                            for (quoted.data.expr, 0..) |atom_id, vi| {
                                const atom = i.ast.nodes[atom_id];

                                // TODO this doesn't work with outher expressions, we'll need a seperate quote function
                                // that does this type of thing recrusively.
                                values[vi] = switch (atom.data) {
                                    .sym => |sym| i.createValue("sym", sym, quoted),
                                    else => try i.eval(atom),
                                };
                            }
                            break :val i.createValue("lst", values, quoted);
                        } else {
                            const symbol = i.bound.get(fn_name);
                            var args = try i.gpa.alloc(Value, expr.len - 1);
                            for (expr[1..], 0..) |atom_id, vi| {
                                const atom = i.ast.nodes[atom_id];
                                args[vi] = try i.eval(atom);
                            }

                            if (symbol) |fun| {
                                switch (fun.data) {
                                    .bif => |proc| {
                                        // TODO if the number of arguments is 1 or we see a rebind
                                        // attach the history to the value
                                        // otherwise just create a new value
                                        var res = proc(i, args);
                                        const hist = try i.gpa.create(HistoryNode);
                                        hist.* = .{
                                            .next = null,
                                            .data = .{
                                                .call = .{
                                                    // TODO fix node pointing
                                                    .node = 999,
                                                    .fn_name = fn_name,
                                                    .args = args,
                                                },
                                            },
                                        };

                                        res.history = hist;
                                        break :val res;
                                    },
                                    else => @panic("i dunno how to call this."),
                                }
                            }
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

fn theia_print(i: *Interp, args: []Interp.Value) Interp.Value {
    _ = i;
    std.debug.assert(args.len == 1);
    std.debug.print("INSPECT: {any}\n", .{args[0].history.*.data.call});
    switch (args[0].data) {
        .str => |str| out_writer.print("{s}\n", .{str}) catch @panic("write failed"),
        .int => |int| out_writer.print("{d}\n", .{int}) catch @panic("write failed"),
        else => @panic("unimplemented"),
    }

    return .nil;
}

fn theia_add(i: *Interp, args: []Interp.Value) Interp.Value {
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
            .int => |d| std.debug.print("{d} - int node {d}\n", .{ i, d }),
            .expr => |e| std.debug.print("{d} - expr node {any}\n", .{ i, e }),
            .root => |r| std.debug.print("{d} - root node {any}\n", .{ i, r }),
            .str => |s| std.debug.print("{d} - str node {s}\n", .{ i, s }),
            .sym => |s| std.debug.print("{d} - sym node {s}\n", .{ i, s }),
        }
    }

    var interp: Interp = .{
        .gpa = allocator,
        .ast = ast,
        .bound = std.StringHashMap(Interp.Value).init(allocator),
    };
    try interp.bound.put("print", .{
        .data = .{ .bif = &theia_print },
        .history = undefined,
    });

    try interp.bound.put("+", .{
        .data = .{ .bif = &theia_add },
        .history = undefined,
    });

    try interp.evalRoot();
    // std.debug.print("{any}", .{ast.nodes});
}
