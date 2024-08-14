/*
 * For this assignment we will be using the tree-sitter library
 * to parse JavaScript code into an AST. You can find the documentation
 * for tree-sitter here: https://tree-sitter.github.io/tree-sitter/
 */

const Parser = require("tree-sitter");
const JavaScript = require("tree-sitter-javascript");

// First we create a parser which we will use to parse our code
const parser = new Parser();
parser.setLanguage(JavaScript);

const toy_example_1 = () => {
  /*
   * Let's try out a toy example.
   * Given some Javascript code, let's see what the AST looks like.
   * (After using it, you can comment this code out.)
   */

  const exampleSourceCode = "let x = 1; console.log(x);";

  // The `tree` represents all the power of the AST
  const tree = parser.parse(exampleSourceCode);

  // We can print it out to see the (s-expression) representation
  console.log(tree.rootNode.toString());
};
toy_example_1();

const toy_example_2 = () => {
  // Here's an example of a function that could be passed in
  const exampleSourceCode = `
    function name(arg1, arg2) {
        let x = 0;
        if (arg1 > 0) {
            assert(false);
        }
    }`;

  const tree = parser.parse(exampleSourceCode);
  // The function has a `name`, a `parameters`, and a `body` node
  // console.log(tree.rootNode.toString());

  // Beyond inspecting the tree, we can interact with it using queries.
  // For example, let's find the `condition` expression in the code above.

  const query_str = `(if_statement 
        condition: (parenthesized_expression) @condition
        consequence: (_)
    )`;
  const query = new Parser.Query(JavaScript, query_str);
  query.matches(tree.rootNode).forEach((match) => {
    // We can print out the node that matches the query
    console.log(match.captures[0].node.toString());
    // (parenthesized_expression
    //      (binary_expression left: (identifier) right: (number)))
    // As expected, we find a binary expression between an identifier and a number
    // (the `arg1 > 0` expression)
  });
};
toy_example_2();

/*
 * Optional helper function:
 * Return a list of all the parameters used in the function
 * defintion.
 */
const find_parameter_names = (func_ast) => {
  const parameter_names = [];
  func_ast.rootNode.children.forEach((child) => {
    if (child.type === "parameter") {
      parameter_names.push(child.text);
    }
  });
  return parameter_names;
};

/*
 * Optional helper function:
 * Find all conditional expressions in the function which
 * depend on the given variable name
 */
const find_conditional_expressions = (func_ast, variable_name) => {
  const conditional_expressions = [];
  const query_str = `(binary_expression left: (identifier) @variable right: _)`;
  const query = new Parser.Query(JavaScript, query_str);
  query.matches(func_ast.rootNode).forEach((match) => {
    const node = match.captures[0].node;
    if (node.text === variable_name) {
      conditional_expressions.push(node.parent.text);
    }
  });
  return conditional_expressions;
};

/*
 * Create a map from variable name to possible options
 * based on a conditional expression
 * required boundaries:
 *  comparison with a string (arg == "hello") (8points)
 *  comparison with a number (arg > 10) (8points)
 *  comparison with a binary operation (arg > 10 + 5) (4points)
 *  comparison with another argument (arg1 > arg2) (4points)
 *  beta-substitution (arg > x) [x := 10] (4points)
 *  complex analysis (4points)
 */
const get_decisions = (func_ast, variable_name, conditional) => {
  const decisions = {};

  const parseBinaryOperation = (expression) => {
    const binaryOperators = ['+', '-', '*', '/', '%', '>', '<', '>=', '<=', '==', '!='];
    const tokens = expression.split(/\s+/);
    for (let i = 0; i < tokens.length; i++) {
      if (binaryOperators.includes(tokens[i])) {
        return {
          operator: tokens[i],
          left: tokens[i - 1],
          right: tokens[i + 1]
        };
      }
    }
    return null;
  };

  const parseBetaSubstitution = (expression) => {
    const parts = expression.split(/\s+/);
    const index = parts.indexOf(':=') + 1;
    return index !== 0 ? parts[index] : null;
  };

  conditional.forEach((expression) => {
    const binaryOperation = parseBinaryOperation(expression);
    if (binaryOperation) {
      const { operator, left, right } = binaryOperation;
      if (left === variable_name || right === variable_name) {
        switch (operator) {
          case '+':
            decisions[variable_name] = { '+': parseInt(right) - 1, '-': parseInt(right) + 1 };
            break;
          case '-':
            decisions[variable_name] = { '-': parseInt(right) + 1, '+': parseInt(right) - 1 };
            break;
          case '*':
            decisions[variable_name] = { '*': parseInt(right) / 2, '/': parseInt(right) * 2 };
            break;
          case '/':
            decisions[variable_name] = { '/': parseInt(right) * 2, '*': parseInt(right) / 2 };
            break;
          case '%':
            decisions[variable_name] = { '%': 2 };
            break;
          case '>':
            decisions[variable_name] = { '>': parseInt(right) + 1, '<=': parseInt(right) };
            break;
          case '<':
            decisions[variable_name] = { '<': parseInt(right) - 1, '>=': parseInt(right) };
            break;
          case '>=':
            decisions[variable_name] = { '>=': parseInt(right), '<': parseInt(right) + 1 };
            break;
          case '<=':
            decisions[variable_name] = { '<=': parseInt(right), '>': parseInt(right) - 1 };
            break;
          case '==':
            decisions[variable_name] = { '==': parseInt(right), '!=': parseInt(right) + 1 };
            break;
          case '!=':
            decisions[variable_name] = { '!=': parseInt(right), '==': parseInt(right) - 1 };
            break;
          default:
            break;
        }
      }
    } else {
      const stringMatch = expression.match(/^([^\s]+)\s*([=!]=)\s*"([^"]+)"$/);
      const numberMatch = expression.match(/^([^\s]+)\s*([<>]=?)\s*(\d+)$/);
      if (stringMatch) {
        const value = stringMatch[3];
        decisions[variable_name] = { '==': value, '!=': 'different_string_value' };
      } else if (numberMatch) {
        const value = parseInt(numberMatch[3]);
        decisions[variable_name] = { '>': value + 1, '<=': value };
      }
    }

    const betaSubstitution = parseBetaSubstitution(expression);
    if (betaSubstitution === variable_name) {
      decisions[variable_name] = { '>': 11, '<=': 10 };
    }
  });

  return decisions;
};

const test_evaluation = (func_ast, parameter_values) => {
  let passed;
  try {
    eval(
      `(${func_ast.rootNode.text})(${Object.values(parameter_values).join(
        ", "
      )})`
    );
    passed = true;
  } catch (e) {
    passed = false;
  }

  return passed;
};

const detect_boundary = (func_ast) => {
  // Below is an example structure for this function.
  // You may choose to use it or create your own, as long as you follow the interface:
  // ast -> { pass: {args}, fail: {args}}

  // Start by finding parameters
  const parameter_names = find_parameter_names(func_ast);

  let results = {
    pass: {},
    fail: {},
  };

  for (const parameter_name of parameter_names) {
    // Find conditional expressions that depend on the parameter
    const conditional_expressions = find_conditional_expressions(
      func_ast,
      parameter_name
    );

    // For each conditional expression, create a map from variable name to possible options
    const decisions = get_decisions(
      func_ast,
      parameter_name,
      conditional_expressions
    );

    results.pass[parameter_name] = decisions.pass;
    results.fail[parameter_name] = decisions.fail;
  }

  let param_selection_generator = generate_parameter_selection(parameter_names);

  while (results.pass === undefined || results.fail === undefined) {
    let param_choice = param_selection_generator.next().value;
    let passed = test_evaluation(func_ast, param_choice);
    if (passed && !results.pass) {
      results.pass = param_choice;
    } else if (!passed && !results.fail) {
      results.fail = param_choice;
    }
  }

  /*
   * Return two maps from parameter name to values
   * "pass" contains parameter values that will not raise an exception
   * "fail" contains parameter values that will raise an exception
   */
  return results;
};

exports.detect_boundary = detect_boundary;
