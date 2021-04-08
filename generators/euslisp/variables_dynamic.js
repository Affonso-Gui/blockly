/**
 * @license
 * Copyright 2018 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for dynamic variable blocks.
 * @author fenichel@google.com (Rachel Fenichel)
 */
'use strict';

goog.provide('Blockly.EusLisp.variablesDynamic');

goog.require('Blockly.EusLisp');
goog.require('Blockly.EusLisp.variables');


// Python is dynamically typed.
Blockly.EusLisp['variables_get_dynamic'] = Blockly.EusLisp['variables_get'];
Blockly.EusLisp['variables_set_dynamic'] = Blockly.EusLisp['variables_set'];
