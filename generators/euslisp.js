/**
 * @license
 * Copyright 2012 Google LLC
 * SPDX-License-Identifier: Apache-2.0
 */

/**
 * @fileoverview Helper functions for generating EusLisp for blocks.
 * @author fraser@google.com (Neil Fraser)
 */
'use strict';

goog.provide('Blockly.EusLisp');

goog.require('Blockly.Generator');
goog.require('Blockly.inputTypes');
goog.require('Blockly.utils.string');

function brack_it() {
  // optional number of arguments
  var args = [].slice.call(arguments);
  args = args.filter(x => x !== '');
  return '(' + args.join(' ') + ')';
};

/**
 * EusLisp code generator.
 * @type {!Blockly.Generator}
 */
Blockly.EusLisp = new Blockly.Generator('EusLisp');

/**
 * List of illegal variable names.
 * This is not intended to be a security feature.  Blockly is 100% client-side,
 * so bypassing this list is trivial.  This is intended to prevent users from
 * accidentally clobbering a built-in object or function.
 * @private
 */
Blockly.EusLisp.addReservedWords(
  "*,**,***,+,++,+++,-," +
  "*asynchronous-stream-functions*,*asynchronous-streams*," +
  "*break-on-warning*,*debug*," +
  "*default-pathname-defaults*,*default-readtable*," +
  "*error-handler*,*error-output*," +
  "*eusdir*,*eustop-hook*,*evalhook*,*exit-on-fatal-error*," +
  "*features*,*history*,*history-index*,*history-max*,*history-sequence*," +
  "*input-line*,*keyword-package*,*line-edit-dispatch*,*lisp-package*," +
  "*load-path*,*load-verbose*,*loaded-modules*,*loader-current-directory*," +
  "*machine*,*modules*,*os-version*,*package*," +
  "*print-base*,*print-case*,*print-circle*,*print-length*,*print-level*,*print-object*,*print-structure*," +
  "*program-name*,*prompt*,*prompt-string*," +
  "*random-state*,*read-base*,*readtable*,*remote-port*,*replevel*,*reptype*," +
  "*select-stream-vector*,*server-streams*,*signal-handlers*," +
  "*standard-input*,*standard-output*,*symbol-input*,*system-package*," +
  "*terminal-io*,*timed-functions*,*timer-job*,*top-selector*,*top-selector-interval*," +
  "*toplevel*,*toplevel-hook*,*traced-functions*,*tracelevel*," +
  "*try-unix*,*unix-package*,*user*,*user-package*," +
  "*world-coords*,*world-coords2*," +
  "pi,pi/2,-2pi,-pi,-pi/2,2pi," +
  "af_inet,af_unix,array,array-dimension-limit,array-rank-limit," +
  "bignum,bit-vector,broadcast-stream,carray,closure,compiled-code,complex," +
  "cons,cstruct,cstructclass,euserror,extended-number,file-stream,float-vector," +
  "foreign-code,foreign-pod,foreign-string,hash-table,integer-vector," +
  "internal-time-units-per-second,io-stream,label-reference," +
  "lisp-implementation-version,load-module," +
  "least-negative-float,least-positive-float," +
  "most-negative-fixnum,most-negative-float,most-positive-fixnum,most-positive-float," +
  "long-float-epsilon,short-float-epsilon,single-float-epsilon," +
  "metaclass,nil,object,package,pathname," +
  "port-selector,propertied-object,queue,ratio,readtable," +
  "socket-address,socket-port,socket-stream,sock_dgram,sock_stream,stream,string," +
  "symbol,t,thread,url-pathname,vector,vectorclass,"
)

/**
 * Order of operation ENUMs.
 * http://docs.python.org/reference/expressions.html#summary
 */
Blockly.EusLisp.ORDER_ATOMIC = 0;            // 0 "" ...
Blockly.EusLisp.ORDER_COLLECTION = 1;        // tuples, lists, dictionaries
Blockly.EusLisp.ORDER_STRING_CONVERSION = 1; // `expression...`
Blockly.EusLisp.ORDER_MEMBER = 2.1;          // . []
Blockly.EusLisp.ORDER_FUNCTION_CALL = 2.2;   // ()
Blockly.EusLisp.ORDER_EXPONENTIATION = 3;    // **
Blockly.EusLisp.ORDER_UNARY_SIGN = 4;        // + -
Blockly.EusLisp.ORDER_BITWISE_NOT = 4;       // ~
Blockly.EusLisp.ORDER_MULTIPLICATIVE = 5;    // * / // %
Blockly.EusLisp.ORDER_ADDITIVE = 6;          // + -
Blockly.EusLisp.ORDER_BITWISE_SHIFT = 7;     // << >>
Blockly.EusLisp.ORDER_BITWISE_AND = 8;       // &
Blockly.EusLisp.ORDER_BITWISE_XOR = 9;       // ^
Blockly.EusLisp.ORDER_BITWISE_OR = 10;       // |
Blockly.EusLisp.ORDER_RELATIONAL = 11;       // in, not in, is, is not,
                                            //     <, <=, >, >=, <>, !=, ==
Blockly.EusLisp.ORDER_LOGICAL_NOT = 12;      // not
Blockly.EusLisp.ORDER_LOGICAL_AND = 13;      // and
Blockly.EusLisp.ORDER_LOGICAL_OR = 14;       // or
Blockly.EusLisp.ORDER_CONDITIONAL = 15;      // if else
Blockly.EusLisp.ORDER_LAMBDA = 16;           // lambda
Blockly.EusLisp.ORDER_NONE = 99;             // (...)

/**
 * List of outer-inner pairings that do NOT require parentheses.
 * @type {!Array.<!Array.<number>>}
 */
Blockly.EusLisp.ORDER_OVERRIDES = [
  // (foo()).bar -> foo().bar
  // (foo())[0] -> foo()[0]
  [Blockly.EusLisp.ORDER_FUNCTION_CALL, Blockly.EusLisp.ORDER_MEMBER],
  // (foo())() -> foo()()
  [Blockly.EusLisp.ORDER_FUNCTION_CALL, Blockly.EusLisp.ORDER_FUNCTION_CALL],
  // (foo.bar).baz -> foo.bar.baz
  // (foo.bar)[0] -> foo.bar[0]
  // (foo[0]).bar -> foo[0].bar
  // (foo[0])[1] -> foo[0][1]
  [Blockly.EusLisp.ORDER_MEMBER, Blockly.EusLisp.ORDER_MEMBER],
  // (foo.bar)() -> foo.bar()
  // (foo[0])() -> foo[0]()
  [Blockly.EusLisp.ORDER_MEMBER, Blockly.EusLisp.ORDER_FUNCTION_CALL],

  // not (not foo) -> not not foo
  [Blockly.EusLisp.ORDER_LOGICAL_NOT, Blockly.EusLisp.ORDER_LOGICAL_NOT],
  // a and (b and c) -> a and b and c
  [Blockly.EusLisp.ORDER_LOGICAL_AND, Blockly.EusLisp.ORDER_LOGICAL_AND],
  // a or (b or c) -> a or b or c
  [Blockly.EusLisp.ORDER_LOGICAL_OR, Blockly.EusLisp.ORDER_LOGICAL_OR]
];

/**
 * Whether the init method has been called.
 * @type {?boolean}
 */
Blockly.EusLisp.isInitialized = false;

/**
 * Initialise the database of variable names.
 * @param {!Blockly.Workspace} workspace Workspace to generate code from.
 * @this {Blockly.Generator}
 */
Blockly.EusLisp.init = function(workspace) {
  // Create a dictionary of definitions to be printed before the code.
  Blockly.EusLisp.definitions_ = Object.create(null);
  // Create a dictionary mapping desired function names in definitions_
  // to actual function names (to avoid collisions with user functions).
  Blockly.EusLisp.functionNames_ = Object.create(null);

  if (!Blockly.EusLisp.variableDB_) {
    Blockly.EusLisp.variableDB_ =
        new Blockly.Names(Blockly.EusLisp.RESERVED_WORDS_);
  } else {
    Blockly.EusLisp.variableDB_.reset();
  }

  Blockly.EusLisp.variableDB_.setVariableMap(workspace.getVariableMap());

  var defvars = [];
  // Add developer variables (not created or named by the user).
  var devVarList = Blockly.Variables.allDeveloperVariables(workspace);
  for (var i = 0; i < devVarList.length; i++) {
    defvars.push(Blockly.EusLisp.variableDB_.getName(devVarList[i],
        Blockly.Names.DEVELOPER_VARIABLE_TYPE));
  }

  // Add user variables, but only ones that are being used.
  var variables = Blockly.Variables.allUsedVarModels(workspace);
  for (var i = 0; i < variables.length; i++) {
    var variable_name = Blockly.EusLisp.variableDB_.
        getName(variables[i].getId(), Blockly.VARIABLE_CATEGORY_NAME)
    defvars.push(brack_it('defvar', variable_name));
  }

  Blockly.EusLisp.definitions_['variables'] = defvars.join('\n');
  this.isInitialized = true;
};

/**
 * Prepend the generated code with the variable definitions.
 * @param {string} code Generated code.
 * @return {string} Completed code.
 */
Blockly.EusLisp.finish = function(code) {
  // Convert the definitions dictionary into a list.
  var imports = [];
  var definitions = [];
  for (var name in Blockly.EusLisp.definitions_) {
    var def = Blockly.EusLisp.definitions_[name];
    if (def.match(/^(from\s+\S+\s+)?import\s+\S+/)) {
      imports.push(def);
    } else {
      definitions.push(def);
    }
  }
  // Clean up temporary data.
  delete Blockly.EusLisp.definitions_;
  delete Blockly.EusLisp.functionNames_;
  Blockly.EusLisp.variableDB_.reset();
  var allDefs = imports.join('\n') + '\n\n' + definitions.join('\n\n');
  return allDefs.replace(/\n\n+/g, '\n\n').replace(/\n*$/, '\n\n\n') + code;
};

/**
 * Naked values are top-level blocks with outputs that aren't plugged into
 * anything.
 * @param {string} line Line of generated code.
 * @return {string} Legal line of code.
 */
// Blockly.EusLisp.scrubNakedValue = function(line) {
//   return line + '\n';
// };

/**
 * Encode a string as a properly escaped Python string, complete with quotes.
 * @param {string} string Text to encode.
 * @return {string} Python string.
 * @protected
 */
Blockly.EusLisp.quote_ = function(string) {
  // Can't use goog.string.quote since % must also be escaped.
  string = string.replace(/\\/g, '\\\\')
                 .replace(/\n/g, '\\\n');

  // Follow the CPython behaviour of repr() for a non-byte string.
  var quote = '\"';
  if (string.indexOf('"') !== -1) {
      string = string.replace(/"/g, '\\\"');
    }
  return quote + string + quote;
};

/**
 * Encode a string as a properly escaped multiline Python string, complete
 * with quotes.
 * @param {string} string Text to encode.
 * @return {string} Python string.
 * @protected
 */
Blockly.EusLisp.multiline_quote_ = function(string) {
  var lines = string.split(/\n/g).map(Blockly.EusLisp.quote_);
  // Join with the following, plus a newline:
  // + '\n' +
  if (lines.length == 1) {
    return lines[0];
  }
  return brack_it('concatenate', 'string', lines.join(" '(#\\Newline) ")) + '\n';
};

/**
 * Common tasks for generating Python from blocks.
 * Handles comments for the specified block and any connected value blocks.
 * Calls any statements following this block.
 * @param {!Blockly.Block} block The current block.
 * @param {string} code The Python code created for this block.
 * @param {boolean=} opt_thisOnly True to generate code for only this statement.
 * @return {string} Python code with comments and subsequent blocks added.
 * @protected
 */
Blockly.EusLisp.scrub_ = function(block, code, opt_thisOnly) {
  var commentCode = '';
  // Only collect comments for blocks that aren't inline.
  if (!block.outputConnection || !block.outputConnection.targetConnection) {
    // Collect comment for this block.
    var comment = block.getCommentText();
    if (comment) {
      comment = Blockly.utils.string.wrap(comment,
          Blockly.EusLisp.COMMENT_WRAP - 3);
      commentCode += Blockly.EusLisp.prefixLines(comment + '\n', ';; ');
    }
    // Collect comments for all value arguments.
    // Don't collect comments for nested statements.
    for (var i = 0; i < block.inputList.length; i++) {
      if (block.inputList[i].type == Blockly.inputTypes.VALUE) {
        var childBlock = block.inputList[i].connection.targetBlock();
        if (childBlock) {
          comment = Blockly.EusLisp.allNestedComments(childBlock);
          if (comment) {
            commentCode += Blockly.EusLisp.prefixLines(comment, '; ');
          }
        }
      }
    }
  }
  var nextBlock = block.nextConnection && block.nextConnection.targetBlock();
  var nextCode = opt_thisOnly ? '' : Blockly.EusLisp.blockToCode(nextBlock);
  var separator = nextCode ? '\n' : '';
  return separator + commentCode + code + separator + nextCode;
};

/**
 * Gets a property and adjusts the value, taking into account indexing, and
 * casts to an integer.
 * @param {!Blockly.Block} block The block.
 * @param {string} atId The property ID of the element to get.
 * @param {number=} opt_delta Value to add.
 * @param {boolean=} opt_negate Whether to negate the value.
 * @return {string|number}
 */
Blockly.EusLisp.getAdjustedInt = function(block, atId, opt_delta, opt_negate) {
  var delta = opt_delta || 0;
  if (block.workspace.options.oneBasedIndex) {
    delta--;
  }
  var defaultAtIndex = block.workspace.options.oneBasedIndex ? '1' : '0';
  var atOrder = delta ? Blockly.EusLisp.ORDER_ADDITIVE :
      Blockly.EusLisp.ORDER_NONE;
  var at = Blockly.EusLisp.valueToCode(block, atId, atOrder) || defaultAtIndex;

  if (Blockly.isNumber(at)) {
    // If the index is a naked number, adjust it right now.
    at = parseInt(at, 10) + delta;
    if (opt_negate) {
      // ensure that a number bigger than 1 is returned
      at = Math.max(at, 1);
      // already used at subtractions
      // at = -at;
    } else {
      // ensure that a non-negative is returned
      at = Math.max(at, 0);
    }
  } else {
    // If the index is dynamic, adjust it in code.
    if (delta > 0) {
      at = 'int(' + at + ' + ' + delta + ')';
    } else if (delta < 0) {
      at = 'int(' + at + ' - ' + -delta + ')';
    } else {
      at = 'int(' + at + ')';
    }
    if (opt_negate) {
      at = '-' + at;
    }
  }
  return at;
};


/**
 * Generate a code string representing the blocks attached to the named
 * statement input. Indent the code according to EusLisp syntax.
 * @param {!Blockly.Block} block The block containing the input.
 * @param {string} name The name of the input.
 * @return {string} Generated code or '' if no blocks are connected.
 */
Blockly.EusLisp.statementToCode = function(block, name) {
  var targetBlock = block.getInputTargetBlock(name);
  var nextBlock = this.nextBlock(block, name);
  var code = this.blockToCode(targetBlock);
  // Value blocks must return code and order of operations info.
  // Statement blocks must only return code.
  if (typeof code != 'string') {
    throw TypeError('Expecting code from statement block: ' +
        (targetBlock && targetBlock.type));
  }

  if (code && nextBlock) {
    code = this.prefixLines(/** @type {string} */ (code), this.INDENT);
  }
  return code;
};

/**
 * Return the next block in the chain.
 * @param {!Blockly.Block} block The block containing the input.
 * @param {string} name The name of the input.
 * @return {!Blockly.Block} Next Block or False if no blocks are connected.
 */
Blockly.EusLisp.nextBlock = function(block, name) {
  var target = block.getInputTargetBlock(name);
  var nextBlock = target && target.nextConnection && target.nextConnection.targetBlock();
  return nextBlock;
};
