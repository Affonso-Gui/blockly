/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for colour blocks.
 * @author fraser@google.com (Neil Fraser)
 */
'use strict';

goog.provide('Blockly.EusLisp.colour');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['colour_picker'] = function(block) {
  // Colour picker.
  var code = Blockly.EusLisp.quote_(block.getFieldValue('COLOUR'));
  return [code, Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['colour_random'] = function(block) {
  // Generate a random colour.
  var code = '(format nil "#~0,6X" (random (1- (expt 2 24))))';
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['colour_rgb'] = function(block) {
  // Compose a colour from RGB components expressed as percentages.
  var functionName = Blockly.EusLisp.provideFunction_(
      'colour-rgb',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + '(r g b)',
       '  (flet ((x255 (x) (round (* 2.55 (min 100 (max 0 x))))))',
       '    (format nil "#~0,2X~0,2X~0,2X" (x255 r) (x255 g) (x255 b))))']);
  var r = Blockly.EusLisp.valueToCode(block, 'RED',
                                     Blockly.EusLisp.ORDER_NONE) || 0;
  var g = Blockly.EusLisp.valueToCode(block, 'GREEN',
                                     Blockly.EusLisp.ORDER_NONE) || 0;
  var b = Blockly.EusLisp.valueToCode(block, 'BLUE',
                                     Blockly.EusLisp.ORDER_NONE) || 0;
  var code = brack_it(functionName, r, g, b);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['colour_blend'] = function(block) {
  // Blend two colours together.
  var functionName = Blockly.EusLisp.provideFunction_(
      'colour-blend',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ +
          '(colour1, colour2, ratio):',
       '  (let ((*read-base* 16)',
       '        (ratio (max (min ratio 1) 0)))',
       '    (flet ((blend-component (start end)',
       '             (round',
       '              (+ (* (read-from-string (subseq colour1 start end)) (- 1 ratio))',
       '                 (* (read-from-string (subseq colour2 start end)) ratio)))))',
       '      (format nil "#~0,2X~0,2X~0,2X"',
       '              (blend-component 1 3) (blend-component 3 5) (blend-component 5 7)))))']);
  var colour1 = Blockly.EusLisp.valueToCode(block, 'COLOUR1',
      Blockly.EusLisp.ORDER_NONE) || '"#000000"';
  var colour2 = Blockly.EusLisp.valueToCode(block, 'COLOUR2',
      Blockly.EusLisp.ORDER_NONE) || '"#000000"';
  var ratio = Blockly.EusLisp.valueToCode(block, 'RATIO',
      Blockly.EusLisp.ORDER_NONE) || 0;
  var code = brack_it(functionName, colour1, colour2, ratio);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};
