/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for procedure blocks.
 * @author fraser@google.com (Neil Fraser)
 */
'use strict';

goog.provide('Blockly.EusLisp.procedures');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['procedures_defreturn'] = function(block) {
  // Define a procedure with a return value.
  var funcName = Blockly.EusLisp.variableDB_.getName(
      block.getFieldValue('NAME'), Blockly.PROCEDURE_CATEGORY_NAME);
  var xfix1 = '';
  if (Blockly.EusLisp.STATEMENT_PREFIX) {
    xfix1 += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_PREFIX, block);
  }
  if (Blockly.EusLisp.STATEMENT_SUFFIX) {
    xfix1 += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_SUFFIX, block);
  }
  if (xfix1) {
    xfix1 = Blockly.EusLisp.prefixLines(xfix1, Blockly.EusLisp.INDENT);
  }
  var loopTrap = '';
  if (Blockly.EusLisp.INFINITE_LOOP_TRAP) {
    loopTrap = Blockly.EusLisp.prefixLines(
        Blockly.EusLisp.injectId(Blockly.EusLisp.INFINITE_LOOP_TRAP, block),
        Blockly.EusLisp.INDENT);
  }
  var branch = Blockly.EusLisp.statementToCode(block, 'STACK');
  var returnValue = Blockly.EusLisp.valueToCode(block, 'RETURN',
      Blockly.EusLisp.ORDER_NONE) || '';
  var xfix2 = '';
  if (branch && returnValue) {
    // After executing the function body, revisit this block for the return.
    xfix2 = xfix1;
  }
  var args = [];
  var variables = block.getVars();
  for (var i = 0; i < variables.length; i++) {
    args[i] = Blockly.EusLisp.variableDB_.getName(variables[i],
        Blockly.VARIABLE_CATEGORY_NAME);
  }
  var code = brack_it('defun', funcName, brack_it(...args),
                      xfix1, loopTrap, branch, xfix2, returnValue);
  code = Blockly.EusLisp.scrub_(block, code);
  // Add % so as not to collide with helper functions in definitions list.
  Blockly.EusLisp.definitions_['%' + funcName] = code;
  return null;
};

// Defining a procedure without a return value uses the same generator as
// a procedure with a return value.
Blockly.EusLisp['procedures_defnoreturn'] =
    Blockly.EusLisp['procedures_defreturn'];

Blockly.EusLisp['procedures_callreturn'] = function(block) {
  // Call a procedure with a return value.
  var funcName = Blockly.EusLisp.variableDB_.getName(block.getFieldValue('NAME'),
      Blockly.PROCEDURE_CATEGORY_NAME);
  var args = [];
  var variables = block.getVars();
  for (var i = 0; i < variables.length; i++) {
    args[i] = Blockly.EusLisp.valueToCode(block, 'ARG' + i,
        Blockly.EusLisp.ORDER_NONE) || 'nil';
  }
  var code = brack_it(funcName, ...args);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['procedures_callnoreturn'] = function(block) {
  // Call a procedure with no return value.
  // Generated code is for a function call as a statement is the same as a
  // function call as a value, with the addition of line ending.
  var tuple = Blockly.EusLisp['procedures_callreturn'](block);
  return tuple[0] + '\n';
};

Blockly.EusLisp['procedures_ifreturn'] = function(block) {
  // Conditionally return value from a procedure.
  var condition = Blockly.EusLisp.valueToCode(block, 'CONDITION',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var value;
  if (block.hasReturnValue_) {
    value = Blockly.EusLisp.valueToCode(block, 'VALUE',
        Blockly.EusLisp.ORDER_NONE) || 'nil';
  } else {
    value = '';
  }
  var code = brack_it('if', condition, brack_it('return', value));
  if (Blockly.EusLisp.STATEMENT_SUFFIX) {
    // Inject any statement suffix here since the regular one at the end
    // will not get executed if the return is triggered.
    code += Blockly.EusLisp.prefixLines(
        Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_SUFFIX, block),
        Blockly.EusLisp.INDENT);
  }
  return code;
};
