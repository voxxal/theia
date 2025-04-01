const std = @import("std");
const token = @import("token.zig");

pub const TokenIndex = token.TokenIndex;
pub const Token = token.Token;
pub const NodeIndex = u32;

pub const Parse = struct {
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

pub const Ast = struct {
    gpa: std.mem.Allocator,
    source: [:0]const u8,
    nodes: []const Node,
    tokens: []const Token,
    root_node: Node,
    errors: []const Parse.Error,

    pub const NodeData = union(enum) {
        root: []const NodeIndex,
        expr: []const NodeIndex,
        int: i32,
        float: f64,
        sym: []const u8,
        str: []const u8,
    };

    pub const Node = struct {
        main_token: TokenIndex,
        data: NodeData,
    };

    pub fn parse(gpa: std.mem.Allocator, source: [:0]const u8) !Ast {
        var tokens: std.ArrayList(Token) = .init(gpa);

        var tokenizer = token.Tokenizer.init(source);
        while (true) {
            const tok = tokenizer.next();
            try tokens.append(tok);
            if (tok.tag == .eof) break;
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
