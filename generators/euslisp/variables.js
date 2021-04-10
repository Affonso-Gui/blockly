/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for variable blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.variables');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['variables_get'] = function(block) {
  // Variable getter.
  var code = Blockly.EusLisp.variableDB_.getName(block.getFieldValue('VAR'),
      Blockly.VARIABLE_CATEGORY_NAME);
  return [code, Blockly.EusLisp.ORDER_VARIABLE];
};

Blockly.EusLisp['variables_set'] = function(block) {
  // Variable setter.
  var argument0 = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var varName = Blockly.EusLisp.variableDB_.getName(block.getFieldValue('VAR'),
      Blockly.VARIABLE_CATEGORY_NAME);
  return brack_it('setq', varName, argument0) + '\n';
};
