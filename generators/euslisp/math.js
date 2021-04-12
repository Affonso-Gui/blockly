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
  } else if (code == -Infinity) {
    code = '*-inf*';
  }
  return [code, Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['math_arithmetic'] = function(block) {
  // Basic arithmetic operators, and power.
  var OPERATORS = {
    'ADD': '+',
    'MINUS': '-',
    'MULTIPLY': '*',
    'DIVIDE': '/',
    'POWER': 'expt',
  };
  var operator = OPERATORS[block.getFieldValue('OP')];
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code = brack_it(operator, argument0, argument1);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_single'] = function(block) {
  // Math operators with single operand.
  var operator = block.getFieldValue('OP');
  var arg = Blockly.EusLisp.valueToCode(block, 'NUM',
        Blockly.EusLisp.ORDER_NONE) || '0';
  switch (operator) {
    case 'NEG':
      var code = brack_it('-', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ABS':
      var code = brack_it('abs', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ROOT':
      var code = brack_it('sqrt', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'LN':
      var code = brack_it('log', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'LOG10':
      var code = brack_it('log', arg, '10');
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'EXP':
      var code = brack_it('exp', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'POW10':
      var code = brack_it('expt', '10', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ROUND':
      var code = brack_it('round', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ROUNDUP':
      var code = brack_it('ceiling', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ROUNDDOWN':
      var code = brack_it('floor', arg);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'SIN':
      var code = brack_it('sin', brack_it('deg2rad', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'COS':
      var code = brack_it('cos', brack_it('deg2rad', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'TAN':
      var code = brack_it('tan', brack_it('deg2rad', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ASIN':
      var code = brack_it('rad2deg', brack_it('asin', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ACOS':
      var code = brack_it('rad2deg', brack_it('acos', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ATAN':
      var code = brack_it('rad2deg', brack_it('atan', arg));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    default:
      throw Error('Unknown math operator: ' + operator);
  }
};

Blockly.EusLisp['math_constant'] = function(block) {
  // Constants: PI, E, the Golden Ratio, sqrt(2), 1/sqrt(2), INFINITY.
  var CONSTANTS = {
    'PI': ['pi', Blockly.EusLisp.ORDER_ATOMIC],
    'E': ['(exp 1)', Blockly.EusLisp.ORDER_FUNCTION_CALL],
    'GOLDEN_RATIO': ['(/ (1+ (sqrt 5)) 2)',
                     Blockly.EusLisp.ORDER_FUNCTION_CALL],
    'SQRT2': ['(sqrt 2)', Blockly.EusLisp.ORDER_FUNCTION_CALL],
    'SQRT1_2': ['(sqrt 1/2)', Blockly.EusLisp.ORDER_FUNCTION_CALL],
    'INFINITY': ['*inf*', Blockly.EusLisp.ORDER_ATOMIC]
  };
  var constant = block.getFieldValue('CONSTANT');
  return CONSTANTS[constant];
};

Blockly.EusLisp['math_number_property'] = function(block) {
  // Check if a number is even, odd, prime, whole, positive, or negative
  // or if it is divisible by certain number. Returns true or false.
  var operator = block.getFieldValue('PROPERTY');
  var number_to_check = Blockly.EusLisp.valueToCode(block, 'NUMBER_TO_CHECK',
      Blockly.EusLisp.ORDER_NONE) || '0';
  switch (operator) {
    case 'EVEN':
      var code = brack_it('evenp', number_to_check);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'ODD':
      var code = brack_it('oddp', number_to_check);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'WHOLE':
      var code = brack_it('zerop', brack_it('mod', number_to_check, 1));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'POSITIVE':
      var code = brack_it('plusp', number_to_check);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'NEGATIVE':
      var code = brack_it('minusp', number_to_check);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'DIVISIBLE_BY':
      var divisor = Blockly.EusLisp.valueToCode(block, 'DIVISOR',
          Blockly.EusLisp.ORDER_NONE);
      // If 'divisor' is some code that evals to 0, Python will raise an error.
      if (!divisor || divisor == '0') {
        return ['nil', Blockly.EusLisp.ORDER_ATOMIC];
      }
      var code = brack_it('zerop', brack_it('mod', number_to_check, divisor));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'PRIME':
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
      var code = brack_it(functionName, number_to_check);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    default:
      throw Error('Unknown math operator: ' + operator);
  }
};

Blockly.EusLisp['math_change'] = function(block) {
  // Add to a variable in place.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'DELTA',
      Blockly.EusLisp.ORDER_NONE) || '0';
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
  var operator = block.getFieldValue('OP');
  var list = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  switch (operator) {
    case 'SUM':
      var code = brack_it('reduce', "#'+", list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'MIN':
      var code = brack_it('reduce', "#'min", list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'MAX':
      var code = brack_it('reduce', "#'max", list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'AVERAGE':
      var code = brack_it('vmean', list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
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
      var code = brack_it(functionName, list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
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
      var code = brack_it(functionName, list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'STD_DEV':
      var code = brack_it('sqrt', brack_it('variance', list));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'RANDOM':
      var functionName = Blockly.EusLisp.provideRandomChoice();
      var code = brack_it(functionName, list);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    default:
      throw Error('Unknown math operator: ' + operator);
  }
};

Blockly.EusLisp['math_modulo'] = function(block) {
  // Remainder computation.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'DIVIDEND',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'DIVISOR',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code = brack_it('mod', argument0, argument1);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['math_constrain'] = function(block) {
  // Constrain a number between two limits.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'LOW',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument2 = Blockly.EusLisp.valueToCode(block, 'HIGH',
      Blockly.EusLisp.ORDER_NONE) || '*inf*';
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
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};
