/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Generating Python for list blocks.
 * @author q.neutron@gmail.com (Quynh Neutron)
 */
'use strict';

goog.provide('Blockly.EusLisp.lists');

goog.require('Blockly.EusLisp');


Blockly.EusLisp['lists_create_empty'] = function(block) {
  // Create an empty list.
  return ['()', Blockly.EusLisp.ORDER_ATOMIC];
};

Blockly.EusLisp['lists_create_with'] = function(block) {
  // Create a list with any number of elements of any type.
  if (block.itemCount_ == 0) {
    return ['()', Blockly.EusLisp.ORDER_ATOMIC];
  }
  var elements = new Array(block.itemCount_);
  for (var i = 0; i < block.itemCount_; i++) {
    elements[i] = Blockly.EusLisp.valueToCode(block, 'ADD' + i,
        Blockly.EusLisp.ORDER_NONE) || 'nil';
  }
  var code = brack_it('list', ...elements);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_repeat'] = function(block) {
  // Create a list with one element repeated.
  var item = Blockly.EusLisp.valueToCode(block, 'ITEM',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var times = Blockly.EusLisp.valueToCode(block, 'NUM',
      Blockly.EusLisp.ORDER_NONE) || '0';
  var code = brack_it('make-list', times, ':initial-element', item);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_length'] = function(block) {
  // String or array length.
  var list = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '()';
  var code = brack_it('length', list);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_isEmpty'] = function(block) {
  // Is the string null or array empty?
  var list = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || '()';
  var code = brack_it('null', list);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_indexOf'] = function(block) {
  // Find an item in the list.
  var item = Blockly.EusLisp.valueToCode(block, 'FIND',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  var list = Blockly.EusLisp.valueToCode(block, 'VALUE',
      Blockly.EusLisp.ORDER_NONE) || 'nil';
  if (block.workspace.options.oneBasedIndex) {
    var errorIndex = '0';
    var firstIndexAdjustment = (name) => `(1+ ${name})`;
    var lastIndexAdjustment = (name, list) => `(- (length ${list}) ${name})`;
  } else {
    var errorIndex = '-1';
    var firstIndexAdjustment = (name) => name;
    var lastIndexAdjustment = (name, list) => `(- (length ${list}) ${name} 1)`;
  }
  if (block.getFieldValue('END') == 'FIRST') {
    var functionName = Blockly.EusLisp.provideFunction_(
        'first-index',
        ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (item lst)',
         " (let ((index (position item lst :test #'equal)))",
         "   (if index",
         `       ${firstIndexAdjustment('index')})`,
         `       ${errorIndex})))`]);
    var code = brack_it(functionName, item, list);
    return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  var functionName = Blockly.EusLisp.provideFunction_(
      'last-index',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (item lst):',
       " (let ((index (position item (reverse lst) :test #'equal)))",
       "     (if index",
       `         ${lastIndexAdjustment('index', 'lst')}`,
       `         ${errorIndex}))`]);
  var code = brack_it(functionName, item, list);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_getIndex'] = function(block) {
  // Get element at index.
  // Note: Until January 2013 this block did not have MODE or WHERE inputs.
  var mode = block.getFieldValue('MODE') || 'GET';
  var where = block.getFieldValue('WHERE') || 'FROM_START';
  var list = Blockly.EusLisp.valueToCode(block, 'VALUE', Blockly.EusLisp.ORDER_NONE) || '()';

  switch (where) {
    case 'FIRST':
      if (mode == 'GET') {
        var code = brack_it('car', list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'GET_REMOVE') {
        var code = brack_it('pop', list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        var code = brack_it('pop', list)
        return code;
      }
      break;
    case 'LAST':
      if (mode == 'GET') {
        var code = brack_it('car', brack_it('last', list));
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'GET_REMOVE') {
        var code = brack_it('prog1',
                            brack_it('last', list),
                            brack_it('setq', list, brack_it('butlast', list)));
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        var code = brack_it('setq', list, brack_it('butlast', list));
        return code;
      }
      break;
    case 'FROM_START':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
      if (mode == 'GET') {
        var code = brack_it('nth', at, list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'GET_REMOVE') {
        var code = brack_it('prog1',
                            brack_it('nth', at, list),
                            brack_it('list-delete', list, at));
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        var code = brack_it('list-delete', list, at);
        return code;
      }
      break;
    case'FROM_END':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT', 1, true);
      at = brack_it('-', brack_it('length', list), at);
      if (mode == 'GET') {
        var code = brack_it('nth', at, list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'GET_REMOVE') {
        var code = brack_it('prog1',
                            brack_it('nth', at, list),
                            brack_it('list-delete', list, at));
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        var code = brack_it('list-delete', list, at);
        return code;
      }
      break;
    case 'RANDOM':
      Blockly.EusLisp.definitions_['import_random'] = 'import random';
      if (mode == 'GET') {
        var code = brack_it('nth', brack_it('random', brack_it('length', list)), list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else {
        var functionName = Blockly.EusLisp.provideFunction_(
            'lists-remove-random-item',
            ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + '(my-list):',
             "  (let ((r (random (length my-list))))",
             "    (prog1 ",
             "        (nth r my-list)",
             "      (list-delete my-list r)))"]);
        var code = brack_it(functionName, list);
        if (mode == 'GET_REMOVE') {
          return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
        } else if (mode == 'REMOVE') {
          return code;
        }
      }
      break;
  }
  throw Error('Unhandled combination (lists_getIndex).');
};

Blockly.EusLisp['lists_setIndex'] = function(block) {
  // Set element at index.
  // Note: Until February 2013 this block did not have MODE or WHERE inputs.
  var list = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || '()';
  var mode = block.getFieldValue('MODE') || 'GET';
  var where = block.getFieldValue('WHERE') || 'FROM_START';
  var value = Blockly.EusLisp.valueToCode(block, 'TO',
      Blockly.EusLisp.ORDER_NONE) || 'nil';

  switch (where) {
    case 'FIRST':
      if (mode == 'SET') {
        var code = brack_it('setf', brack_it('car', list), value);
        return code;
      } else if (mode == 'INSERT') {
        var code = brack_it('push', value, list);
        return code;
      }
      break;
    case 'LAST':
        if (mode == 'SET') {
          var code = brack_it('setf', brack_it('car', brack_it('last', list)), value);
          return code;
        } else if (mode == 'INSERT') {
          var code = brack_it('list-insert', value, brack_it('length', list), list);
          return code;
        }
      break;
    case 'FROM_START':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
        if (mode == 'SET') {
          var code = brack_it('setf', brack_it('nth', at, list), value);
          return code;
        } else if (mode == 'INSERT') {
          var code = brack_it('list-insert', value, at, list);
          return code;
        }
      break;
    case 'FROM_END':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT', 1, true);
      at = brack_it('-', brack_it('length', list), at);
        if (mode == 'SET') {
          var code = brack_it('setf', brack_it('nth', at, list), value);
          return code;
        } else if (mode == 'INSERT') {
          var code = brack_it('list-insert', value, at, list);
          return code;
        }
      break;
    case 'RANDOM':
        if (mode == 'SET') {
          var code = `(setf (nth (random (length ${list})) ${list}) ${value})`;
          return code;
        } else if (mode == 'INSERT') {
          var code = `(list-insert ${value} (random (length ${list})) ${list})`;
          return code;
        }
      break;
  }
  throw Error('Unhandled combination (lists_setIndex).');
};

Blockly.EusLisp['lists_getSublist'] = function(block) {
  // Get sublist.
  var list = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || '()';
  var where1 = block.getFieldValue('WHERE1');
  var where2 = block.getFieldValue('WHERE2');
  switch (where1) {
    case 'FROM_START':
      var at1 = Blockly.EusLisp.getAdjustedInt(block, 'AT1');
      break;
    case 'FROM_END':
      var at1 = Blockly.EusLisp.getAdjustedInt(block, 'AT1', 1, true);
      at1 = brack_it('-', brack_it('length', list), at1);
      break;
    case 'FIRST':
      var at1 = '0';
      break;
    default:
      throw Error('Unhandled option (lists_getSublist)');
  }
  switch (where2) {
    case 'FROM_START':
      var at2 = Blockly.EusLisp.getAdjustedInt(block, 'AT2', 1);
      break;
    case 'FROM_END':
      var at2 = Blockly.EusLisp.getAdjustedInt(block, 'AT2', 0);
      if (at2 == 0) {
        at2 = '';
      } else {
        at2 = brack_it('-', brack_it('length', list), at2);
      }
      break;
    case 'LAST':
      var at2 = '';
      break;
    default:
      throw Error('Unhandled option (lists_getSublist)');
  }
  var code = brack_it('subseq', list, at1, at2);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_sort'] = function(block) {
  // Block for sorting a list.
  var list = (Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || '()');
  var type = block.getFieldValue('TYPE');
  var direction = block.getFieldValue('DIRECTION');

  switch (type) {
    case 'NUMERIC':
      var code = `(sort (copy-list ${list}) #'${direction? '<=' : '>='} ` +
        "#'(lambda (x) (if (numberp x) x 0)))";
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'TEXT':
      var code = `(sort (copy-list ${list}) #'string${direction? '<=' : '>='} ` +
        "#'string)";
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'IGNORE_CASE':
      var code = `(sort (copy-list ${list}) #'string${direction? '<=' : '>='} ` +
        "#'(lambda (x) (string-downcase (string x))))";
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    default:
      throw Error('Unhandled option (lists_sort)');
  }
};

Blockly.EusLisp['lists_split'] = function(block) {
  // Block for splitting text into a list, or joining a list into text.
  var mode = block.getFieldValue('MODE');
  if (mode == 'SPLIT') {
    var value_input = Blockly.EusLisp.valueToCode(block, 'INPUT',
        Blockly.EusLisp.ORDER_NONE) || '""';
    var value_delim = Blockly.EusLisp.valueToCode(block, 'DELIM',
        Blockly.EusLisp.ORDER_NONE);
    var code = value_input + '.split(' + value_delim + ')';
  } else if (mode == 'JOIN') {
    var value_input = Blockly.EusLisp.valueToCode(block, 'INPUT',
        Blockly.EusLisp.ORDER_NONE) || '()';
    var value_delim = Blockly.EusLisp.valueToCode(block, 'DELIM',
        Blockly.EusLisp.ORDER_NONE) || '""';
    var functionName = Blockly.EusLisp.provideFunction_(
      'lists-join',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + '(my-list delim):',
      "  (apply #'concatenate string",
      "         (string (car my-list))",
      "         (mapcar #'(lambda (a) (concatenate string delim (string a)))",
      "                 (cdr my-list))))"]);
    var code = brack_it(functionName, value_input, value_delim);
  } else {
    throw Error('Unknown mode: ' + mode);
  }
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_reverse'] = function(block) {
  // Block for reversing a list.
  var list = Blockly.EusLisp.valueToCode(block, 'LIST',
      Blockly.EusLisp.ORDER_NONE) || '()';
  var code = brack_it('reverse', list);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};
