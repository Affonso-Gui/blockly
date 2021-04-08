/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for math blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.math');

goog.require('Blockly.EusLisp');


// If any new block imports any library, add that library name here.
Blockly.EusLisp.addReservedWords('math,random,Number');

Blockly.EusLisp['math_number'] = function(block) {
  // Numeric value.
  var code = Number(block.getFieldValue('NUM'));
  var order;
  if (code == Infinity) {
    code = '*inf*';
    order = Blockly.EusLisp.ORDER_FUNCTION_CALL;
  } else if (code == -Infinity) {
    code = '*-inf*';
    order = Blockly.EusLisp.ORDER_UNARY_SIGN;
  } else {
    order = code < 0 ? Blockly.EusLisp.ORDER_UNARY_SIGN :
            Blockly.EusLisp.ORDER_ATOMIC;
  }
  return [code, order];
};

Blockly.EusLisp['math_arithmetic'] = function(block) {
  // Basic arithmetic operators, and power.
  var OPERATORS = {
    'ADD': ['+', Blockly.EusLisp.ORDER_ADDITIVE],
    'MINUS': ['-', Blockly.EusLisp.ORDER_ADDITIVE],
    'MULTIPLY': ['*', Blockly.EusLisp.ORDER_MULTIPLICATIVE],
    'DIVIDE': ['/', Blockly.EusLisp.ORDER_MULTIPLICATIVE],
    'POWER': ['expt', Blockly.EusLisp.ORDER_EXPONENTIATION]
  };
  var tuple = OPERATORS[block.getFieldValue('OP')];
  var operator = tuple[0];
  var order = tuple[1];
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A', order) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B', order) || '0';
  var code = brack_it(operator, argument0, argument1);
  return [code, order];
  // In case of 'DIVIDE', division between integers returns different results
  // in Python 2 and 3. However, is not an issue since Blockly does not
  // guarantee identical results in all languages.  To do otherwise would
  // require every operator to be wrapped in a function call.  This would kill
  // legibility of the generated code.
};

Blockly.EusLisp['math_single'] = function(block) {
  // Math operators with single operand.
  var operator = block.getFieldValue('OP');
  var code;
  var arg;
  if (operator == 'NEG') {
    // Negation is a special case given its different operator precedence.
    code = Blockly.EusLisp.valueToCode(block, 'NUM',
        Blockly.EusLisp.ORDER_UNARY_SIGN) || '0';
    return [brack_it('-', code), Blockly.EusLisp.ORDER_UNARY_SIGN];
  }
  if (operator == 'SIN' || operator == 'COS' || operator == 'TAN') {
    arg = Blockly.EusLisp.valueToCode(block, 'NUM',
        Blockly.EusLisp.ORDER_MULTIPLICATIVE) || '0';
  } else {
    arg = Blockly.EusLisp.valueToCode(block, 'NUM',
        Blockly.EusLisp.ORDER_NONE) || '0';
  }
  // First, handle cases which generate values that don't need parentheses
  // wrapping the code.
  switch (operator) {
    case 'ABS':
      code = brack_it('abs', arg);
      break;
    case 'ROOT':
      code = brack_it('sqrt', arg);
      break;
    case 'LN':
      code = brack_it('log', arg);
      break;
    case 'LOG10':
      code = brack_it('log', arg, '10');
      break;
    case 'EXP':
      code = brack_it('exp', arg);
      break;
    case 'POW10':
      code = brack_it('expt', '10', arg);
      break;
    case 'ROUND':
      code = brack_it('round', arg);
      break;
    case 'ROUNDUP':
      code = brack_it('ceiling', arg);
      break;
    case 'ROUNDDOWN':
      code = brack_it('floor', arg);
      break;
    case 'SIN':
      code = brack_it('sin', brack_it('deg2rad', arg));
      break;
    case 'COS':
      code = brack_it('cos', brack_it('deg2rad', arg));
      break;
    case 'TAN':
      code = brack_it('tan', brack_it('deg2rad', arg));
      break;
  }
  if (code) {
    return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  // Second, handle cases which generate values that may need parentheses
  // wrapping the code.
  switch (operator) {
    case 'ASIN':
      code = brack_it('rad2deg', brack_it('asin', arg));
      break;
    case 'ACOS':
      code = brack_it('rad2deg', brack_it('acos', arg));
      break;
    case 'ATAN':
      code = brack_it('rad2deg', brack_it('atan', arg));
      break;
    default:
      throw Error('Unknown math operator: ' + operator);
  }
  return [code, Blockly.EusLisp.ORDER_MULTIPLICATIVE];
};

Blockly.EusLisp['math_constant'] = function(block) {
  // Constants: PI, E, the Golden Ratio, sqrt(2), 1/sqrt(2), INFINITY.
  var CONSTANTS = {
    'PI': ['pi', Blockly.EusLisp.ORDER_MEMBER],
    'E': ['(exp 1)', Blockly.EusLisp.ORDER_MEMBER],
    'GOLDEN_RATIO': ['(/ (1+ (sqrt 5)) 2)',
                     Blockly.EusLisp.ORDER_MULTIPLICATIVE],
    'SQRT2': ['(sqrt 2)', Blockly.EusLisp.ORDER_MEMBER],
    'SQRT1_2': ['(sqrt 1/2)', Blockly.EusLisp.ORDER_MEMBER],
    'INFINITY': ['*inf*', Blockly.EusLisp.ORDER_ATOMIC]
  };
  var constant = block.getFieldValue('CONSTANT');
  return CONSTANTS[constant];
};

Blockly.EusLisp['math_number_property'] = function(block) {
  // Check if a number is even, odd, prime, whole, positive, or negative
  // or if it is divisible by certain number. Returns true or false.
  var number_to_check = Blockly.EusLisp.valueToCode(block, 'NUMBER_TO_CHECK',
      Blockly.EusLisp.ORDER_MULTIPLICATIVE) || '0';
  var dropdown_property = block.getFieldValue('PROPERTY');
  var code;
  if (dropdown_property == 'PRIME') {
    var functionName = Blockly.EusLisp.provideFunction_(
        'primep',
        ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (n)',
         ";; https://en.wikipedia.org/wiki/Primality_test#Naive_methods",
         "  (cond",
         "    ((or (= n 2) (= n 3)) t)",
         "    ((or (not (numberp n))",
         "         (<= n 1)",
         "         (not (zerop (mod n 1)))",
         "         (zerop (mod n 2))",
         "         (zerop (mod n 3)))",
         "     nil)",
         "    (t",
         "     (do ((x 6 (+ x 6)))",
         "         ((> x (1+ (sqrt n))) t)",
         "       (if (or (zerop (mod n (1- x)))",
         "               (zerop (mod n (1+ x))))",
         "           (return nil))))))"]);
    code = brack_it(functionName, number_to_check);
    return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  switch (dropdown_property) {
    case 'EVEN':
      code = brack_it('evenp', number_to_check);
      break;
    case 'ODD':
      code = brack_it('oddp', number_to_check);
      break;
    case 'WHOLE':
      code = brack_it('zerop', brack_it('mod', number_to_check, 1));
      break;
    case 'POSITIVE':
      code = brack_it('plusp', number_to_check);
      break;
    case 'NEGATIVE':
      code = brack_it('minusp', number_to_check);
      break;
    case 'DIVISIBLE_BY':
      var divisor = Blockly.EusLisp.valueToCode(block, 'DIVISOR',
          Blockly.EusLisp.ORDER_MULTIPLICATIVE);
      // If 'divisor' is some code that evals to 0, Python will raise an error.
      if (!divisor || divisor == '0') {
        return ['nil', Blockly.EusLisp.ORDER_ATOMIC];
      }
      code = brack_it('zerop', brack_it('mod', number_to_check, divisor));
      break;
  }
  return [code, Blockly.EusLisp.ORDER_RELATIONAL];
};

Blockly.EusLisp['math_change'] = function(block) {
  // Add to a variable in place.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'DELTA',
      Blockly.EusLisp.ORDER_ADDITIVE) || '0';
  var varName = Blockly.EusLisp.variableDB_.getName(block.getFieldValue('VAR'),
      Blockly.VARIABLE_CATEGORY_NAME);
  if (argument0 == '1') {
    var code = brack_it('incf', varName);
    return code;
  }
  var code = brack_it('incf', varName, argument0);
  return code;
};

// Rounding functions have a single operand.
Blockly.EusLisp['math_round'] = Blockly.EusLisp['math_single'];
// Trigonometry functions have a single operand.
Blockly.EusLisp['math_trig'] = Blockly.EusLisp['math_single'];

Blockly.EusLisp['math_on_list'] = function(block) {
  // Math functions for lists.
  var func = block.getFieldValue('OP');
  var list = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var code;
  switch (func) {
    case 'SUM':
      code = brack_it('reduce', "#'+", list);
      break;
    case 'MIN':
      code = brack_it('reduce', "#'min", list);
      break;
    case 'MAX':
      code = brack_it('reduce', "#'max", list);
      break;
    case 'AVERAGE':
      code = brack_it('vmean', brack_it('remove-if-not', "#'numberp", list));
      break;
    case 'MEDIAN':
      var functionName = Blockly.EusLisp.provideFunction_(
          'median',
          // This operation excludes null values:
          // (median (list nil nil 1 3)) == 2.0.
          ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list)',
           "  (let ((local-list (sort (remove-if-not #'numberp my-list) #'<=)))",
           "    (if local-list",
           "        (if (evenp (length local-list))",
           "            (/ (+ (nth (/ (length local-list) 2) local-list)",
           "                  (nth (1- (/ (length local-list) 2)) local-list))",
           "               2.0)",
           "            (nth (/ (1- (length local-list)) 2) local-list)))))"]);
      code = brack_it(functionName, list);
      break;
    case 'MODE':
      var functionName = Blockly.EusLisp.provideFunction_(
          'modes',
          // As a list of numbers can contain more than one mode,
          // the returned result is provided as an array.
          // Mode of [3, 'x', 'x', 1, 1, 2, '3'] -> ['x', 1].
          ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list)',
           "   (let* ((counts (mapcar #'(lambda (a) (cons a (count a my-list :test #'equal)))",
           "                          (remove-duplicates my-list :test #'equal)))",
           "          (max-count (reduce #'max (mapcar #'cdr counts))))",
           "     (mapcan #'(lambda (a) (if (= (cdr a) max-count) (list (car a)))) counts)))"]);
      code = brack_it(functionName, list);
      break;
    case 'STD_DEV':
      code = brack_it('sqrt', brack_it('variance', list));
      break;
    case 'RANDOM':
      var functionName = Blockly.EusLisp.provideFunction_(
        'random-choice',
        ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list)',
         "  (nth (random (length my-list)) my-list))"]);
      code = brack_it(functionName, list);
      break;
    default:
      throw Error('Unknown operator: ' + func);
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_modulo'] = function(block) {
  // Remainder computation.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'DIVIDEND',
      Blockly.EusLisp.ORDER_MULTIPLICATIVE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'DIVISOR',
      Blockly.EusLisp.ORDER_MULTIPLICATIVE) || '0';
  var code = brack_it('mod', argument0, argument1);
  return [code, Blockly.EusLisp.ORDER_MULTIPLICATIVE];
};

Blockly.EusLisp['math_constrain'] = function(block) {
  // Constrain a number between two limits.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'LOW',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument2 = Blockly.EusLisp.valueToCode(block, 'HIGH',
      Blockly.EusLisp.ORDER_NONE) || 'float(\'inf\')';
  var code = brack_it('min', brack_it('max', argument0, argument1), argument2);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_random_int'] = function(block) {
  // Random integer between [X] and [Y].
  var argument0 = Blockly.EusLisp.valueToCode(block, 'FROM',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'TO',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code;
  if (argument0 == '0') {
    var code = brack_it('random', argument1);
  } else {
    var code = brack_it('+', argument0, brack_it('random', brack_it('-', argument1, argument0)));
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_random_float'] = function(block) {
  // Random fraction between 0 and 1.
  return ['(random 1.0)', Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_atan2'] = function(block) {
  // Arctangent of point (X, Y) in degrees from -180 to 180.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'X',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'Y',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code = brack_it('rad2deg', brack_it('atan2', argument1, argument0));
  return [code, Blockly.EusLisp.ORDER_MULTIPLICATIVE];
};
