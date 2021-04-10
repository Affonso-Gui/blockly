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
        'first-position',
        ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (item my-list)',
         " (let ((index (position item my-list :test #'equal)))",
         `   (if index ${firstIndexAdjustment('index')} ${errorIndex}))))`]);
    var code = brack_it(functionName, item, list);
    return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
  }
  var functionName = Blockly.EusLisp.provideFunction_(
      'last-position',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (item my-list)',
       " (let ((index (position item (reverse my-list) :test #'equal)))",
       `     (if index ${lastIndexAdjustment('index', 'my-list')} ${errorIndex}))`]);
  var code = brack_it(functionName, item, list);
  return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
};

Blockly.EusLisp['lists_getIndex'] = function(block) {
  // Get element at index.
  // Note: Until January 2013 this block did not have MODE or WHERE inputs.
  var mode = block.getFieldValue('MODE') || 'GET';
  var where = block.getFieldValue('WHERE') || 'FROM_START';
  var tuple = Blockly.EusLisp.valueToCodeOrder(block, 'VALUE') ||
      ['()', Blockly.EusLisp.ORDER_ATOMIC];
  var list = tuple[0];
  var order = tuple[1];

  var providePopN = function () {
    var functionName = Blockly.EusLisp.provideFunction_(
      'pop-n',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ +
       ' (my-list n &key from-end)',
       '  (let ((n (if from-end (- (length my-list) n) n)))',
       '    (prog1 (nth n my-list)',
       '      (if (zerop n)',
       '          (pop my-list)',
       '          (list-delete my-list n)))))']);
    return functionName;
  };

  switch (where) {
    case 'FIRST':
      if (mode == 'GET') {
        var code = brack_it('car', list);
        return [code, Blockly.EusLisp.ORDER_SETF];
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
        return [code, Blockly.EusLisp.ORDER_SETF];
      } else if (mode == 'GET_REMOVE') {
        var functionName = providePopN();
        var code = brack_it(functionName, list, '0', ':from-end t');
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        switch (order) {
          case Blockly.EusLisp.ORDER_VARIABLE:
            var code = brack_it('setq', list, brack_it('butlast', list));
            return code;
          case Blockly.EusLisp.ORDER_SETF:
            var code = brack_it('setf', list, brack_it('butlast', list));
            return code;
          default:
            var code = brack_it('butlast', list);
            return code;
        }
      }
      break;
    case 'FROM_START':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
      if (mode == 'GET') {
        var code = brack_it('nth', at, list);
        return [code, Blockly.EusLisp.ORDER_SETF];
      } else if (mode == 'GET_REMOVE') {
        var functionName = providePopN();
        var code = brack_it(functionName, list, at);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else if (mode == 'REMOVE') {
        var functionName = providePopN();
        var code = brack_it(functionName, list, at);
        return code;
      }
      break;
    case'FROM_END':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
      if (mode == 'GET') {
        var code = brack_it('nth', at, brack_it('reverse', list));
        return [code, Blockly.EusLisp.ORDER_SETF];
      } else {
        // REMOVE || GET_REMOVE
        var functionName = providePopN();
        var code = brack_it(functionName, list, at, ':from-end t');
        if (mode == 'GET_REMOVE') {
          return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
        } else if (mode == 'REMOVE') {
          return code;
        }
      }
      break;
    case 'RANDOM':
      if (mode == 'GET') {
        var functionName = Blockly.EusLisp.provideFunction_(
          'random-choice',
          ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list)',
           "  (nth (random (length my-list)) my-list))"]);
        var code = brack_it(functionName, list);
        return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
      } else {
        // REMOVE || GET_REMOVE
        providePopN();
        var functionName = Blockly.EusLisp.provideFunction_(
            'pop-random',
            ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list)',
             '  (let ((n (random (length my-list))))',
             '    (pop-n my-list n)))']);
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

  var providePushN = function () {
    var functionName = Blockly.EusLisp.provideFunction_(
      'push-n',
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ +
       ' (item my-list n &key from-end)',
       '  (let ((n (if from-end (- (length my-list) n) n)))',
       '    (if (zerop n)',
       '        (push item my-list)',
       '        (list-insert item n my-list))))']);
    return functionName;
  };

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
          var functionName = providePushN();
          var code = brack_it(functionName, value, list, '0', ':from-end t');
          return code;
        }
      break;
    case 'FROM_START':
      var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
        if (mode == 'SET') {
          var code = brack_it('setf', brack_it('nth', at, list), value);
          return code;
        } else if (mode == 'INSERT') {
          var functionName = providePushN();
          var code = brack_it(functionName, value, list, at);
          return code;
        }
      break;
    case 'FROM_END':
        if (mode == 'SET') {
          var at = Blockly.EusLisp.getAdjustedInt(block, 'AT', 0, true);
          at = brack_it('-', brack_it('length', list), at);
          var code = brack_it('setf', brack_it('nth', at, list), value);
          return code;
        } else if (mode == 'INSERT') {
          var at = Blockly.EusLisp.getAdjustedInt(block, 'AT');
          var functionName = providePushN();
          var code = brack_it(functionName, value, list, at, ':from-end t');
          return code;
        }
      break;
    case 'RANDOM':
        if (mode == 'SET') {
          var code = `(setf (nth (random (length ${list})) ${list}) ${value})`;
          return code;
        } else if (mode == 'INSERT') {
          providePushN();
          var functionName = Blockly.EusLisp.provideFunction_(
            'push-random',
            ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (item my-list)',
             '  (let ((n (random (length my-list))))',
             '    (push-n item my-list n)))']);
          var code = brack_it(functionName, value, list);
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
  var tuple = Blockly.EusLisp.valueToCodeOrder(block, 'LIST') ||
      ['()', Blockly.EusLisp.ORDER_ATOMIC];
  var list = tuple[0];
  var order = tuple[1];
  var type = block.getFieldValue('TYPE');
  var direction = block.getFieldValue('DIRECTION');

  if (order == Blockly.EusLisp.ORDER_VARIABLE) {
    list = brack_it('copy-list', list);
  }
  switch (type) {
    case 'NUMERIC':
      var code = `(sort ${list} #'${direction? '<=' : '>='} ` +
        "#'(lambda (x) (if (numberp x) x 0)))";
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'TEXT':
      var code = `(sort ${list} #'string${direction? '<=' : '>='} ` +
        "#'string)";
      return [code, Blockly.EusLisp.ORDER_FUNCTION_CALL];
    case 'IGNORE_CASE':
      var code = `(sort ${list} #'string${direction? '<=' : '>='} ` +
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
      ['(defun ' + Blockly.EusLisp.FUNCTION_NAME_PLACEHOLDER_ + ' (my-list delim)',
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
