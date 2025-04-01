const std = @import("std");
const token = @import("token.zig");
const ast = @import("ast.zig");
const interp = @import("interp.zig");
const builtins = @import("builtins.zig");

const Tokenizer = token.Tokenizer;
const Token = token.Token;
const Ast = ast.Ast;
const Interp = interp.Interp;
const TheiaBuiltin = builtins.TheiaBuiltin;

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const expectEqualStrings = testing.expectEqualStrings;

// Test tokenization
test "tokenizer basic functionality" {
    const source = "(+ 1 2)";
    var tokenizer = Tokenizer.init(source);

    // Test first token: (
    var tok = tokenizer.next();
    try expectEqual(Token.Tag.lp, tok.tag);

    // Test second token: +
    tok = tokenizer.next();
    try expectEqual(Token.Tag.sym, tok.tag);
    try expectEqualStrings("+", source[tok.loc.start..tok.loc.end]);

    // Test third token: 1
    tok = tokenizer.next();
    try expectEqual(Token.Tag.int, tok.tag);
    try expectEqualStrings("1", source[tok.loc.start..tok.loc.end]);

    // Test fourth token: 2
    tok = tokenizer.next();
    try expectEqual(Token.Tag.int, tok.tag);
    try expectEqualStrings("2", source[tok.loc.start..tok.loc.end]);

    // Test fifth token: )
    tok = tokenizer.next();
    try expectEqual(Token.Tag.rp, tok.tag);

    // Test EOF
    tok = tokenizer.next();
    try expectEqual(Token.Tag.eof, tok.tag);
}

test "tokenizer handles strings" {
    const source = "\"hello world\"";
    var tokenizer = Tokenizer.init(source);

    var tok = tokenizer.next();
    try expectEqual(Token.Tag.str, tok.tag);
    try expectEqualStrings("\"hello world\"", source[tok.loc.start..tok.loc.end]);

    tok = tokenizer.next();
    try expectEqual(Token.Tag.eof, tok.tag);
}

test "tokenizer handles quotes" {
    const source = "'symbol";
    var tokenizer = Tokenizer.init(source);

    var tok = tokenizer.next();
    try expectEqual(Token.Tag.quote, tok.tag);

    tok = tokenizer.next();
    try expectEqual(Token.Tag.sym, tok.tag);
    try expectEqualStrings("symbol", source[tok.loc.start..tok.loc.end]);

    tok = tokenizer.next();
    try expectEqual(Token.Tag.eof, tok.tag);
}

// Test parsing
test "parser basic functionality" {
    const source = "(+ 1 2)";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    // Check root node
    try expectEqual(ast.Ast.NodeData.root, ast_tree.root_node.data);
    try expectEqual(@as(usize, 1), ast_tree.root_node.data.root.len);

    // Check expression node
    const expr_node = ast_tree.nodes[ast_tree.root_node.data.root[0]];
    try expectEqual(ast.Ast.NodeData.expr, expr_node.data);
    try expectEqual(@as(usize, 3), expr_node.data.expr.len);

    // Check + symbol
    const plus_node = ast_tree.nodes[expr_node.data.expr[0]];
    try expectEqual(ast.Ast.NodeData.sym, plus_node.data);
    try expectEqualStrings("+", plus_node.data.sym);

    // Check 1
    const one_node = ast_tree.nodes[expr_node.data.expr[1]];
    try expectEqual(ast.Ast.NodeData.int, one_node.data);
    try expectEqual(@as(i32, 1), one_node.data.int);

    // Check 2
    const two_node = ast_tree.nodes[expr_node.data.expr[2]];
    try expectEqual(ast.Ast.NodeData.int, two_node.data);
    try expectEqual(@as(i32, 2), two_node.data.int);
}

test "parser handles nested expressions" {
    const source = "(+ 1 (+ 2 3))";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    // Check root node
    try expectEqual(ast.Ast.NodeData.root, ast_tree.root_node.data);
    try expectEqual(@as(usize, 1), ast_tree.root_node.data.root.len);

    // Check outer expression node
    const outer_expr_node = ast_tree.nodes[ast_tree.root_node.data.root[0]];
    try expectEqual(ast.Ast.NodeData.expr, outer_expr_node.data);
    try expectEqual(@as(usize, 3), outer_expr_node.data.expr.len);

    // Check inner expression node
    const inner_expr_index = outer_expr_node.data.expr[2];
    const inner_expr_node = ast_tree.nodes[inner_expr_index];
    try expectEqual(ast.Ast.NodeData.expr, inner_expr_node.data);
    try expectEqual(@as(usize, 3), inner_expr_node.data.expr.len);

    // Check inner + symbol
    const inner_plus_node = ast_tree.nodes[inner_expr_node.data.expr[0]];
    try expectEqual(ast.Ast.NodeData.sym, inner_plus_node.data);
    try expectEqualStrings("+", inner_plus_node.data.sym);

    // Check 2
    const two_node = ast_tree.nodes[inner_expr_node.data.expr[1]];
    try expectEqual(ast.Ast.NodeData.int, two_node.data);
    try expectEqual(@as(i32, 2), two_node.data.int);

    // Check 3
    const three_node = ast_tree.nodes[inner_expr_node.data.expr[2]];
    try expectEqual(ast.Ast.NodeData.int, three_node.data);
    try expectEqual(@as(i32, 3), three_node.data.int);
}

// Test nodeToValue function
test "nodeToValue converts AST nodes to Values" {
    const source = "(+ 1 2)";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    // Convert the expression node to a Value
    const expr_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(expr_node_index);

    // Check that it's a list
    try expectEqual(interp.Interp.ValueData.lst, value.data);
    try expectEqual(@as(usize, 3), value.data.lst.len);

    // Check the + symbol
    try expectEqual(interp.Interp.ValueData.sym, value.data.lst[0].data);
    try expectEqualStrings("+", value.data.lst[0].data.sym);

    // Check the 1
    try expectEqual(interp.Interp.ValueData.int, value.data.lst[1].data);
    try expectEqual(@as(i32, 1), value.data.lst[1].data.int);

    // Check the 2
    try expectEqual(interp.Interp.ValueData.int, value.data.lst[2].data);
    try expectEqual(@as(i32, 2), value.data.lst[2].data.int);
}

// Test eval function with the new Value parameter
test "eval evaluates Values" {
    const source = "(+ 1 2)";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    try TheiaBuiltin.attach(&interpreter);

    // Convert the expression node to a Value
    const expr_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(expr_node_index);

    // Evaluate the Value
    const result = try interpreter.eval(value);

    // Check that the result is 3
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 3), result.data.int);
}

test "eval handles simple values" {
    const source = "42";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    // Convert the int node to a Value
    const int_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(int_node_index);

    // Evaluate the Value
    const result = try interpreter.eval(value);

    // Check that the result is 42
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 42), result.data.int);
}

test "eval handles symbols" {
    const source = "true";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    // Convert the symbol node to a Value
    const sym_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(sym_node_index);

    // Evaluate the Value
    const result = try interpreter.eval(value);

    // Check that the result is true
    try expectEqual(interp.Interp.ValueData.bool, result.data);
    try expect(result.data.bool);
}

// Test built-in functions
test "built-in add function" {
    const source = "(+ 1 2 3 4)";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    try TheiaBuiltin.attach(&interpreter);

    // Convert the expression node to a Value
    const expr_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(expr_node_index);

    // Evaluate the Value
    const result = try interpreter.eval(value);

    // Check that the result is 10
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 10), result.data.int);
}

// Test quote macro
test "quote macro" {
    const source = "'(1 2 3)";
    const allocator = testing.allocator;

    const ast_tree = try Ast.parse(allocator, source);
    defer {
        for (ast_tree.nodes) |node| {
            if (node.data == .expr) allocator.free(node.data.expr);
            if (node.data == .root) allocator.free(node.data.root);
        }
        allocator.free(ast_tree.nodes);
        allocator.free(ast_tree.tokens);
        allocator.free(ast_tree.errors);
    }

    var interpreter = try Interp.init(allocator, ast_tree);
    defer interpreter.bound.deinit();

    try TheiaBuiltin.attach(&interpreter);

    // Convert the quote node to a Value
    const quote_node_index = ast_tree.root_node.data.root[0];
    const value = try interpreter.nodeToValue(quote_node_index);

    // Evaluate the Value
    const result = try interpreter.eval(value);

    // Check that the result is a list with 3 elements
    try expectEqual(interp.Interp.ValueData.lst, result.data);
    try expectEqual(@as(usize, 3), result.data.lst.len);

    // Check the elements
    try expectEqual(interp.Interp.ValueData.int, result.data.lst[0].data);
    try expectEqual(@as(i32, 1), result.data.lst[0].data.int);

    try expectEqual(interp.Interp.ValueData.int, result.data.lst[1].data);
    try expectEqual(@as(i32, 2), result.data.lst[1].data.int);

    try expectEqual(interp.Interp.ValueData.int, result.data.lst[2].data);
    try expectEqual(@as(i32, 3), result.data.lst[2].data.int);
}
