/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for logic blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.logic');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['controls_if'] = function(block) {
  // If/elseif/else condition.
  var n = 0;
  var branchCode, conditionCode;
  if (Blockly.EusLisp.STATEMENT_PREFIX) {
    // Automatic prefix insertion is switched off for this block.  Add manually.
    code += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_PREFIX, block);
  }
  do {
    ++n;
  } while (block.getInput('IF' + n));

  if (n == 1) {
    conditionCode = Blockly.EusLisp.valueToCode(block, 'IF0',
        Blockly.EusLisp.ORDER_NONE) || 'nil';
    branchCode = Blockly.EusLisp.statementToCode(block, 'DO0');
    if (Blockly.EusLisp.nextBlock(block, 'DO0')) {
      branchCode = brack_it('progn', branchCode);
    }
    var elseCode = Blockly.EusLisp.statementToCode(block, 'ELSE');
    if (Blockly.EusLisp.nextBlock(block, 'ELSE')) {
      elseCode = brack_it('progn', elseCode);
    }
    var code = brack_it('if', conditionCode, branchCode, elseCode);
    return code;
  }

  n = 0;
  var code = '(cond \n';
  do {
    conditionCode = Blockly.EusLisp.valueToCode(block, 'IF' + n,
        Blockly.EusLisp.ORDER_NONE) || 'nil';
    branchCode = Blockly.EusLisp.statementToCode(block, 'DO' + n);
    if (Blockly.EusLisp.STATEMENT_SUFFIX) {
      branchCode = Blockly.EusLisp.prefixLines(
          Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_SUFFIX, block),
          Blockly.EusLisp.INDENT) + branchCode;
    }
    if (n != 0) code += '\n';
    code += Blockly.EusLisp.INDENT + brack_it(conditionCode, branchCode);
    ++n;
  } while (block.getInput('IF' + n));

  if (block.getInput('ELSE') || Blockly.EusLisp.STATEMENT_SUFFIX) {
    branchCode = Blockly.EusLisp.statementToCode(block, 'ELSE');
    if (Blockly.EusLisp.STATEMENT_SUFFIX) {
      branchCode = Blockly.EusLisp.prefixLines(
          Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_SUFFIX, block),
          Blockly.EusLisp.INDENT) + branchCode;
    }
    code += '\n';
    code += Blockly.EusLisp.INDENT + brack_it('t', branchCode);
  }
  code += ')';
  return code;
};

Blockly.EusLisp['controls_ifelse'] = Blockly.EusLisp['controls_if'];

Blockly.EusLisp['logic_compare'] = function(block) {
  // Comparison operator.
  var OPERATORS = {
    'EQ': 'equal',
    'NEQ': 'equal',
    'LT': '<',
    'LTE': '<=',
    'GT': '>',
    'GTE': '>='
  };
  var operator = OPERATORS[block.getFieldValue('OP')];
  var order = Blockly.EusLisp.ORDER_RELATIONAL;
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A', order) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B', order) || '0';
  var code = brack_it(operator, argument0, argument1);
  if (block.getFieldValue('OP') == 'NEQ') {
    code = brack_it('not', code);
  }
  return [code, order];
};

Blockly.EusLisp['logic_operation'] = function(block) {
  // Operations 'and', 'or'.
  var operator = (block.getFieldValue('OP') == 'AND') ? 'and' : 'or';
  var order = (operator == 'and') ? Blockly.EusLisp.ORDER_LOGICAL_AND :
      Blockly.EusLisp.ORDER_LOGICAL_OR;
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A', order);
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B', order);
  if (!argument0 && !argument1) {
    // If there are no arguments, then the return value is false.
    argument0 = 'nil';
    argument1 = 'nil';
  } else {
    // Single missing arguments have no effect on the return value.
    var defaultArgument = (operator == 'and') ? 't' : 'nil';
    if (!argument0) {
      argument0 = defaultArgument;
    }
    if (!argument1) {
      argument1 = defaultArgument;
    }
  }

  var code = brack_it(operator, argument0, argument1);
  return [code, order];
};

Blockly.EusLisp['logic_negate'] = function(block) {
  // Negation.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'BOOL',
      Blockly.EusLisp.ORDER_LOGICAL_NOT) || 't';
  var code = brack_it('not', argument0);
  return [code, Blockly.EusLisp.ORDER_LOGICAL_NOT];
};

Blockly.EusLisp['logic_boolean'] = function(block) {
  // Boolean values true and false.
  var code = (block.getFieldValue('BOOL') == 'TRUE') ? 't' : 'nil';
  return [code, Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['logic_null'] = function(block) {
  // Null data type.
  return ['nil', Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['logic_ternary'] = function(block) {
  // Ternary operator.
  var value_if = Blockly.EusLisp.valueToCode(block, 'IF',
      Blockly.EusLisp.ORDER_CONDITIONAL) || 'nil';
  var value_then = Blockly.EusLisp.valueToCode(block, 'THEN',
      Blockly.EusLisp.ORDER_CONDITIONAL) || 'nil';
  var value_else = Blockly.EusLisp.valueToCode(block, 'ELSE',
      Blockly.EusLisp.ORDER_CONDITIONAL) || 'nil';
  var code = brack_it('if', value_if, value_then, value_else);
  return [code, Blockly.EusLisp.ORDER_CONDITIONAL];
};
