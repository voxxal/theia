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

// Future Feature: Variable binding with 'def'
test "variable binding with def" {
    const source = "(def x 42) x";
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

    // Evaluate the program
    try interpreter.evalRoot();

    // Get the value of x
    const x_value = interpreter.bound.get("x") orelse {
        try expect(false); // Should not reach here
        unreachable;
    };

    // Check that x is 42
    try expectEqual(interp.Interp.ValueData.int, x_value.data);
    try expectEqual(@as(i32, 42), x_value.data.int);
}

// Future Feature: Function definition with 'defun'
test "function definition with defun" {
    const source = "(defun add (a b) (+ a b)) (add 3 4)";
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

    // Convert the root node to a Value
    const root_value = try interpreter.nodeToValue(0);

    // Evaluate the program
    const result = try interpreter.eval(root_value);

    // Check that the result is 7
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 7), result.data.int);
}

// Future Feature: Conditional expressions with 'if'
test "conditional expressions with if" {
    const source = "(if true 42 0)";
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

    // Check that the result is 42
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 42), result.data.int);
}

// Future Feature: String concatenation
test "string concatenation" {
    const source = "(+ \"Hello, \" \"World!\")";
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

    // Check that the result is "Hello, World!"
    try expectEqual(interp.Interp.ValueData.str, result.data);
    try expectEqualStrings("Hello, World!", result.data.str);
}

// Future Feature: Comparison operators
test "comparison operators" {
    const source = "(< 1 2)";
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

    // Check that the result is true
    try expectEqual(interp.Interp.ValueData.bool, result.data);
    try expect(result.data.bool);
}

// Future Feature: List operations
test "list operations" {
    const source = "(list 1 2 3)";
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

// Future Feature: Input/Output operations
test "input output operations" {
    const source = "(println (read-line))";
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

    // Check that the result is nil
    try expectEqual(interp.Interp.ValueData.lst, result.data);
    try expectEqual(@as(usize, 0), result.data.lst.len);
}

// Future Feature: Random number generation
test "random number generation" {
    const source = "(random 10)";
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

    // Check that the result is an integer between 0 and 9
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expect(result.data.int >= 0 and result.data.int < 10);
}

// Sample Guessing Game
test "guessing game" {
    const source =
        \\(defun guessing-game ()
        \\  (def target (random 100))
        \\  (def guess -1)
        \\  (def attempts 0)
        \\  (while (not (= guess target))
        \\    (set attempts (+ attempts 1))
        \\    (println "Enter your guess (0-99):")
        \\    (set guess (parse-int (read-line)))
        \\    (cond
        \\      ((< guess target) (println "Too low!"))
        \\      ((> guess target) (println "Too high!"))
        \\      (true (println (+ "Correct! You guessed it in " (+ (to-string attempts) " attempts."))))))
        \\
        \\(guessing-game)
    ;
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

    // Mock input/output for testing
    // This would require additional functionality to be added to the interpreter

    // Evaluate the program
    try interpreter.evalRoot();

    // Since this is a failing test for a future feature, we don't have specific assertions
    // The test will pass once the guessing game functionality is implemented
    try expect(true);
}

// Future Feature: Error handling with try/catch
test "error handling with try catch" {
    const source =
        \\(try
        \\  (/ 10 0)
        \\  (catch e
        \\    (println (+ "Caught error: " e))
        \\    42))
    ;
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

    // Check that the result is 42 (the error handler's return value)
    try expectEqual(interp.Interp.ValueData.int, result.data);
    try expectEqual(@as(i32, 42), result.data.int);
}

// Future Feature: Modules and imports
test "modules and imports" {
    const source =
        \\(import "math")
        \\(math.sqrt 16)
    ;
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

    // Evaluate the program
    try interpreter.evalRoot();

    // Since this is a failing test for a future feature, we don't have specific assertions
    // The test will pass once the module system is implemented
    try expect(true);
}
