/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for loop blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.loops');

goog.require('Blockly.EusLisp');


/**
 * This is the text used to implement a <pre>continue</pre>.
 * It is also used to recognise <pre>continue</pre>s in generated code so that
 * the appropriate label can be put at the end of the loop body.
 * @const {string}
 */
Blockly.EusLisp.CONTINUE_STATEMENT = '(go :continue)';

/**
 * If the loop body contains a "(go :continue)" statement, add a continue label
 * to the loop body. Slightly inefficient, as continue labels will be generated
 * in all outer loops, but this is safer than duplicating the logic of
 * blockToCode.
 *
 * @param {string} branch Generated code of the loop body
 * @return {string} Generated label or '' if unnecessary
 * @private
 */
Blockly.EusLisp.addContinueLabel_ = function(branch) {
  if (branch.indexOf(Blockly.EusLisp.CONTINUE_STATEMENT) != -1) {
    // False positives are possible (e.g. a string literal), but are harmless.
    // var code = `\n(tagbody ${branch}\n :continue)`;
    var code = '\n' + brack_it('tagbody', branch, '\n :continue');
    code = Blockly.EusLisp.prefixLines(code, Blockly.EusLisp.INDENT);
    return code;
  }
  return branch;
};


Blockly.EusLisp['controls_repeat_ext'] = function(block) {
  // Repeat n times.
  if (block.getField('TIMES')) {
    // Internal number.
    var repeats = String(Number(block.getFieldValue('TIMES'), 10));
  } else {
    // External number.
    var repeats = Blockly.EusLisp.valueToCode(block, 'TIMES',
        Blockly.EusLisp.ORDER_NONE) || '0';
  }
  var branch = Blockly.EusLisp.statementToCode(block, 'DO');
  branch = Blockly.EusLisp.addLoopTrap(branch, block);
  branch = Blockly.EusLisp.addContinueLabel_(branch);
  var loopVar = Blockly.EusLisp.variableDB_.getDistinctName(
      'count', Blockly.VARIABLE_CATEGORY_NAME);
  var code = brack_it('dotimes', brack_it(loopVar, repeats), branch);
  return code;
};

Blockly.EusLisp['controls_repeat'] = Blockly.EusLisp['controls_repeat_ext'];

Blockly.EusLisp['controls_whileUntil'] = function(block) {
  // Do while/until loop.
  var until = block.getFieldValue('MODE') == 'UNTIL';
  var argument0 = Blockly.EusLisp.valueToCode(block, 'BOOL',
      until ? Blockly.EusLisp.ORDER_LOGICAL_NOT :
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var branch = Blockly.EusLisp.statementToCode(block, 'DO');
  branch = Blockly.EusLisp.addLoopTrap(branch, block);
  branch = Blockly.EusLisp.addContinueLabel_(branch);
  if (until) {
    argument0 = brack_it('not', argument0);
  }
  var code = brack_it('while', argument0, branch);
  return code;
};

Blockly.EusLisp['controls_for'] = function(block) {
  // For loop.
  var variable0 = Blockly.EusLisp.variableDB_.getName(
      block.getFieldValue('VAR'), Blockly.VARIABLE_CATEGORY_NAME);
  var argument0 = Blockly.EusLisp.valueToCode(block, 'FROM',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var argument1 = Blockly.EusLisp.valueToCode(block, 'TO',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var increment = Blockly.EusLisp.valueToCode(block, 'BY',
      Blockly.EusLisp.ORDER_NONE) || '1';
  var branch = Blockly.EusLisp.statementToCode(block, 'DO');
  branch = Blockly.EusLisp.addLoopTrap(branch, block);
  branch = Blockly.EusLisp.addContinueLabel_(branch);
  // Check for whitespace to cover for single statements wrapped in continue label
  if (branch && !(/^\s/.test(branch) || Blockly.EusLisp.nextBlock(block, 'DO'))) {
    branch = '\n' + Blockly.EusLisp.INDENT + branch;
  }

  var code;
  if (Blockly.isNumber(argument0) && Blockly.isNumber(argument1) &&
      Blockly.isNumber(increment)) {
    // All arguments are simple numbers.
    var step = Math.abs(Number(increment));
    var up = Number(argument0) <= Number(argument1);

    if (argument0 == 0 && step == 1 && up) {
      code = `(dotimes (${variable0} ${argument1})` +
             `${branch})`;
      return code;
    }
    code = `(do ((${variable0} ${argument0} (${up?'+':'-'} ${variable0} ${step})))` + '\n' +
           `    ((${up?'>':'<'} ${variable0} ${argument1}))` +
           `${branch})`;
    return code;
  }
  if (Blockly.isNumber(argument0) && Blockly.isNumber(argument1)) {
    var up = Number(argument0) <= Number(argument1);
    var incVar = Blockly.EusLisp.variableDB_.getDistinctName(
        variable0 + '_inc', Blockly.VARIABLE_CATEGORY_NAME);

    code = `(do* ((${incVar} (abs ${increment}))` + '\n      ' +
           `(${variable0} ${argument0} (${up?'+':'-'} ${variable0} ${incVar})))` +
           '\n     ' +
           `((${up?'>':'<'} ${variable0} ${argument1}))` +
           `${branch})`;
    return code;
  }

  // Cache non-trivial values to variables to prevent repeated look-ups.
  var startVar;
  if (!argument0.match(/^\w+$/) && !Blockly.isNumber(argument0)) {
    startVar = Blockly.EusLisp.variableDB_.getDistinctName(
        variable0 + '_start', Blockly.VARIABLE_CATEGORY_NAME);
  }
  var endVar;
  if (!argument1.match(/^\w+$/) && !Blockly.isNumber(argument1)) {
    endVar = Blockly.EusLisp.variableDB_.getDistinctName(
        variable0 + '_end', Blockly.VARIABLE_CATEGORY_NAME);
  }
  // Determine loop direction at start, in case one of the bounds
  // changes during loop execution.
  var incVar = Blockly.EusLisp.variableDB_.getDistinctName(
      variable0 + '_inc', Blockly.VARIABLE_CATEGORY_NAME);
  var step, stepNegative;
  if (Blockly.isNumber(increment)) {
    step = Math.abs(increment) + '';
    stepNegative = '-' + step;
  } else {
    step = brack_it('abs', increment);
    stepNegative = brack_it('-', increment);
  }
  var opVar = Blockly.EusLisp.variableDB_.getDistinctName(
      variable0 + '_op', Blockly.VARIABLE_CATEGORY_NAME);

  code = `(do* (${startVar? brack_it(startVar, argument0) + '\n      ' : ''}` +
         `${endVar? brack_it(endVar, argument1) + '\n      ' : ''}` +
         `(${incVar} (if (<= ${startVar? startVar : argument0} ${endVar? endVar : argument1}) ` +
         `${step} ${stepNegative}))` + '\n      ' +
         `(${opVar} (if (plusp ${incVar}) #'> #'<))` + '\n      ' +
         `(${variable0} ${startVar? startVar : argument0} (+ ${variable0} ${incVar})))` +
         '\n     ' +
         `((funcall ${opVar} ${variable0} ${endVar? endVar : argument1}))` +
         `${branch})`;
  return code;
};

Blockly.EusLisp['controls_forEach'] = function(block) {
  // For each loop.
  var variable0 = Blockly.EusLisp.variableDB_.getName(
      block.getFieldValue('VAR'), Blockly.VARIABLE_CATEGORY_NAME);
  var argument0 = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_RELATIONAL) || 'nil';
  var branch = Blockly.EusLisp.statementToCode(block, 'DO');
  branch = Blockly.EusLisp.addLoopTrap(branch, block);
  branch = Blockly.EusLisp.addContinueLabel_(branch);
  var code = brack_it('dolist', brack_it(variable0, argument0), branch);
  return code;
};

Blockly.EusLisp['controls_flow_statements'] = function(block) {
  // Flow statements: continue, break.
  var xfix = '';
  if (Blockly.EusLisp.STATEMENT_PREFIX) {
    // Automatic prefix insertion is switched off for this block.  Add manually.
    xfix += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_PREFIX, block);
  }
  if (Blockly.EusLisp.STATEMENT_SUFFIX) {
    // Inject any statement suffix here since the regular one at the end
    // will not get executed if the break/continue is triggered.
    xfix += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_SUFFIX, block);
  }
  if (Blockly.EusLisp.STATEMENT_PREFIX) {
    var loop = Blockly.Constants.Loops
        .CONTROL_FLOW_IN_LOOP_CHECK_MIXIN.getSurroundLoop(block);
    if (loop && !loop.suppressPrefixSuffix) {
      // Inject loop's statement prefix here since the regular one at the end
      // of the loop will not get executed if 'continue' is triggered.
      // In the case of 'break', a prefix is needed due to the loop's suffix.
      xfix += Blockly.EusLisp.injectId(Blockly.EusLisp.STATEMENT_PREFIX, loop);
    }
  }
  switch (block.getFieldValue('FLOW')) {
    case 'BREAK':
      return xfix + '(return)';
    case 'CONTINUE':
      return xfix + Blockly.EusLisp.CONTINUE_STATEMENT;
  }
  throw Error('Unknown flow statement.');
};
