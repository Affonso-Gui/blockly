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
  do ++n; while (block.getInput('IF' + n));

  if (n == 1) {
    conditionCode = Blockly.EusLisp.valueToCode(block, 'IF0',
        Blockly.EusLisp.ORDER_NONE) || 'nil';
    branchCode = Blockly.EusLisp.statementToCode(block, 'DO0');
    var elseCode = Blockly.EusLisp.statementToCode(block, 'ELSE');

    var doNext = Blockly.EusLisp.nextBlock(block, 'DO0');
    var elseNext = Blockly.EusLisp.nextBlock(block, 'ELSE');

    if (doNext && !elseCode) {
      var code = brack_it('when', conditionCode, branchCode);
      return code;
    }
    if (!branchCode && elseCode) {
      var code = brack_it('unless', conditionCode, elseCode);
      return code;
    }
    if (doNext || elseNext) {
      if (doNext) branchCode = brack_it('progn', branchCode);
      if (elseNext) elseCode = brack_it('progn', elseCode);
      branchCode = Blockly.EusLisp.prefixLines('\n' + branchCode, Blockly.EusLisp.INDENT);
      elseCode = Blockly.EusLisp.prefixLines('\n' + elseCode, Blockly.EusLisp.INDENT);
    }

    var code = brack_it('if', conditionCode, branchCode, elseCode);
    return code;
  }

  n = 0;
  var code = '(cond' + '\n';
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
    code += Blockly.EusLisp.prefixLines(
      brack_it(conditionCode, branchCode),
      Blockly.EusLisp.INDENT);
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
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code = brack_it(operator, argument0, argument1);
  if (block.getFieldValue('OP') == 'NEQ') {
    code = brack_it('not', code);
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['logic_operation'] = function(block) {
  // Operations 'and', 'or'.
  var operator = (block.getFieldValue('OP') == 'AND') ? 'and' : 'or';
  var argument0 = Blockly.EusLisp.valueToCode(block, 'A', Blockly.EusLisp.ORDER_NONE);
  var argument1 = Blockly.EusLisp.valueToCode(block, 'B', Blockly.EusLisp.ORDER_NONE);
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
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['logic_negate'] = function(block) {
  // Negation.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'BOOL',
      Blockly.EusLisp.ORDER_NONE) || 't';
  var code = brack_it('not', argument0);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
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
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var value_then = Blockly.EusLisp.valueToCode(block, 'THEN',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var value_else = Blockly.EusLisp.valueToCode(block, 'ELSE',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var code = brack_it('if', value_if, value_then, value_else);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};
