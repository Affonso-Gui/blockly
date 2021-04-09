/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for text blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.texts');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['text'] = function(block) {
  // Text value.
  var code = Blockly.EusLisp.quote_(block.getFieldValue('TEXT'));
  return [code, Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['text_multiline'] = function(block) {
  // Text value.
  var code = Blockly.EusLisp.multiline_quote_(block.getFieldValue('TEXT'));
  var order = code.startsWith('(') != -1 ? Blockly.EusLisp.ORDER_FUNCTION_CALL :
      Blockly.EusLisp.ORDER_ATOMIC;
  return [code, order];
};

/**
 * Enclose the provided value in 'str(...)' function.
 * Leave string literals alone.
 * @param {string} value Code evaluating to a value.
 * @return {[string, number]} Array containing code evaluating to a string and
 *    the order of the returned code.
 * @private
 */
Blockly.EusLisp.text.forceString_ = function(value) {
  if (Blockly.EusLisp.text.forceString_.strRegExp.test(value)) {
    return [value, Blockly.EusLisp.ORDER_ATOMIC];
  }
  return [brack_it('string', value), Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

/**
 * Regular expression to detect a single-quoted string literal.
 */
Blockly.EusLisp.text.forceString_.strRegExp = /^\s*'([^']|\\')*'\s*$/;

Blockly.EusLisp['text_join'] = function(block) {
  // Create a string made up of any number of elements of any type.
  //Should we allow joining by '-' or ',' or any other characters?
  switch (block.itemCount_) {
    case 0:
      return ['""', Blockly.EusLisp.ORDER_ATOMIC];
      break;
    case 1:
      var element = Blockly.EusLisp.valueToCode(block, 'ADD0',
              Blockly.EusLisp.ORDER_NONE) || '""';
      var codeAndOrder = Blockly.EusLisp.text.forceString_(element);
      return codeAndOrder;
      break;
    case 2:
      var element0 = Blockly.EusLisp.valueToCode(block, 'ADD0',
          Blockly.EusLisp.ORDER_NONE) || '""';
      var element1 = Blockly.EusLisp.valueToCode(block, 'ADD1',
          Blockly.EusLisp.ORDER_NONE) || '""';
      var code = brack_it('concatenate', 'string',
          Blockly.EusLisp.text.forceString_(element0)[0],
          Blockly.EusLisp.text.forceString_(element1)[0]);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      break;
    default:
      var elements = [];
      for (var i = 0; i < block.itemCount_; i++) {
        elements[i] = Blockly.EusLisp.valueToCode(block, 'ADD' + i,
                Blockly.EusLisp.ORDER_NONE) || '""';
      }
      var code = brack_it('concatenate', 'string', ...elements);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
};

Blockly.EusLisp['text_append'] = function(block) {
  // Append to a variable in place.
  var varName = Blockly.EusLisp.variableDB_.getName(block.getFieldValue('VAR'),
      Blockly.VARIABLE_CATEGORY_NAME);
  var value = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it('concatenate', 'string', brack_it('string', varName), 
      Blockly.EusLisp.text.forceString_(value)[0]) + '\n';
  return code;
};

Blockly.EusLisp['text_length'] = function(block) {
  // Is the string null or array empty?
  var text = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it('length', text);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_isEmpty'] = function(block) {
  // Is the string null or array empty?
  var text = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it('null-string-p', text);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_indexOf'] = function(block) {
  // Search the text for a substring.
  // Should we allow for non-case sensitive???
  var operator = block.getFieldValue('END') == 'FIRST' ? 'find' : 'rfind';
  var substring = Blockly.EusLisp.valueToCode(block, 'FIND',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var text = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = text + '.' + operator + '(' + substring + ')';
  if (block.workspace.options.oneBasedIndex) {
    return [code + ' + 1', Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_charAt'] = function(block) {
  // Get letter at index.
  // Note: Until January 2013 this block did not have the WHERE input.
  var where = block.getFieldValue('WHERE') || 'FROM_START';
  var text = Blockly.EusLisp.valueToCode(block, 'VALUE', Blockly.EusLisp.ORDER_NONE) || '""';
  switch (where) {
    case 'FIRST':
      var code = brack_it('elt', text, 0);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'LAST':
      var code = brack_it('elt', text, brack_it('1-', brack_it('length', text)));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'FROM_START':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
      var code = brack_it('elt', text, at);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'FROM_END':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT', 1, true);
      var code = brack_it('elt', text, brack_it('-', brack_it('length', text), at));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'RANDOM':
      var code = brack_it('elt', text, brack_it('random', brack_it('length', text)));
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  throw Error('Unhandled option (text_charAt).');
};

Blockly.EusLisp['text_getSubstring'] = function(block) {
  // Get substring.
  var where1 = block.getFieldValue('WHERE1');
  var where2 = block.getFieldValue('WHERE2');
  var text = Blockly.EusLisp.valueToCode(block, 'STRING',
      Blockly.EusLisp.ORDER_NONE) || '""';
  switch (where1) {
    case 'FROM_START':
      var at1 = Blockly.EusLisp.getAdjustedInt(block, 'AT1');
      if (at1 == '0') {
        at1 = '';
      }
      break;
    case 'FROM_END':
      var at1 = Blockly.EusLisp.getAdjustedInt(block, 'AT1', 1, true);
      break;
    case 'FIRST':
      var at1 = '';
      break;
    default:
      throw Error('Unhandled option (text_getSubstring)');
  }
  switch (where2) {
    case 'FROM_START':
      var at2 = Blockly.EusLisp.getAdjustedInt(block, 'AT2', 1);
      break;
    case 'FROM_END':
      var at2 = Blockly.EusLisp.getAdjustedInt(block, 'AT2', 0, true);
      // Ensure that if the result calculated is 0 that sub-sequence will
      // include all elements as expected.
      if (!Blockly.isNumber(String(at2))) {
        Blockly.EusLisp.definitions_['import_sys'] = 'import sys';
        at2 += ' or sys.maxsize';
      } else if (at2 == '0') {
        at2 = '';
      }
      break;
    case 'LAST':
      var at2 = '';
      break;
    default:
      throw Error('Unhandled option (text_getSubstring)');
  }
  var code = brack_it('subseq', text, at1, at2);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_changeCase'] = function(block) {
  // Change capitalization.
  var text = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  switch (Blockly.EusLisp.valueToCode(block, 'TEXT')) {
    case 'UPPERCASE':
      var code = brack_it('string-upcase', text);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'LOWERCASE':
      var code = brack_it('string-downcase', text);
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'TITLECASE':
    // Title case is not a native Dart function.  Define one.
      var functionName = Blockly.Dart.provideFunction_(
        'text_toTitleCase',
        ['String ' + Blockly.Dart.FUNCTION_NAME_PLACEHOLDER_ +
         '(String str) {',
         '  RegExp exp = new RegExp(r\'\\b\');',
         '  List<String> list = str.split(exp);',
         '  final title = new StringBuffer();',
         '  for (String part in list) {',
         '    if (part.length > 0) {',
         '      title.write(part[0].toUpperCase());',
         '      if (part.length > 0) {',
         '        title.write(part.substring(1).toLowerCase());',
         '      }',
         '    }',
         '  }',
         '  return title.toString();',
         '}']);
      var code = functionName + '(' + text + ')';
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
};

Blockly.EusLisp['text_trim'] = function(block) {
  // Trim spaces.
  var OPERATORS = {
    'LEFT': 'string-left-trim',
    'RIGHT': 'string-trim',
    'BOTH': 'string-right-trim'
  };
  var operator = OPERATORS[block.getFieldValue('MODE')];
  var text = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it(operator, '" "', text);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_print'] = function(block) {
  // Print statement.
  var msg = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it('print', msg);
  return code;
};

Blockly.EusLisp['text_prompt_ext'] = function(block) {
  // Prompt function.
  var functionName = Blockly.EusLisp.provideFunction_(
      'text-prompt',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + '(msg)',
       "  (princ msg)",
       "  (finish-output)",
       "  (read-line)"]);
  if (block.getField('TEXT')) {
    // Internal message.
    var msg = Blockly.EusLisp.quote_(block.getFieldValue('TEXT'));
  } else {
    // External message.
    var msg = Blockly.EusLisp.valueToCode(block, 'TEXT',
        Blockly.EusLisp.ORDER_NONE) || '""';
  }
  var code = brack_it(functionName, msg);
  var toNumber = block.getFieldValue('TYPE') == 'NUMBER';
  if (toNumber) {
    code = brack_it('read_from_string', code);
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_prompt'] = Blockly.EusLisp['text_prompt_ext'];

Blockly.EusLisp['text_count'] = function(block) {
  var text = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var sub = Blockly.EusLisp.valueToCode(block, 'SUB',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = text + '.count(' + sub + ')';
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_replace'] = function(block) {
  var text = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var from = Blockly.EusLisp.valueToCode(block, 'FROM',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var to = Blockly.EusLisp.valueToCode(block, 'TO',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = text + '.replace(' + from + ', ' + to + ')';
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['text_reverse'] = function(block) {
  var text = Blockly.EusLisp.valueToCode(block, 'TEXT',
      Blockly.EusLisp.ORDER_NONE) || '""';
  var code = brack_it('reverse', text);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};
