// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Belt_MutableStack from "rescript/lib/es6/belt_MutableStack.js";

function encode(s) {
  var n = s.codePointAt(0);
  if (n !== undefined) {
    return n;
  } else {
    return Pervasives.failwith("cant get code point from string");
  }
}

function decode(prim) {
  return String.fromCodePoint(prim);
}

var Char = {
  encode: encode,
  decode: decode,
};

function _push(stack, s) {
  Belt_MutableStack.push(stack, encode(s));
}

function populate(stack, a) {
  a.forEach(function (e) {
    Belt_MutableStack.push(stack, encode(e));
  });
}

function init(a) {
  var stack = Belt_MutableStack.make(undefined);
  populate(stack, a);
  return stack;
}

var Stack = {
  Char: Char,
  make: Belt_MutableStack.make,
  clear: Belt_MutableStack.clear,
  copy: Belt_MutableStack.copy,
  push: Belt_MutableStack.push,
  popUndefined: Belt_MutableStack.popUndefined,
  pop: Belt_MutableStack.pop,
  topUndefined: Belt_MutableStack.topUndefined,
  top: Belt_MutableStack.top,
  isEmpty: Belt_MutableStack.isEmpty,
  size: Belt_MutableStack.size,
  forEachU: Belt_MutableStack.forEachU,
  forEach: Belt_MutableStack.forEach,
  dynamicPopIterU: Belt_MutableStack.dynamicPopIterU,
  dynamicPopIter: Belt_MutableStack.dynamicPopIter,
  _push: _push,
  populate: populate,
  init: init,
};

var re = /^move\s(?<amount>\d+)\sfrom\s(?<from>\d+)\sto\s(?<to>\d+)$/;

function make(s) {
  var e = re.exec(s);
  if (e === null) {
    return Pervasives.failwith("can not parse command");
  }
  var groups = e.groups;
  var n = Belt_Int.fromString(groups.from);
  var n$1 = Belt_Int.fromString(groups.to);
  var n$2 = Belt_Int.fromString(groups.amount);
  return {
    from: n !== undefined ? n : Pervasives.failwith("can not parse integer"),
    to: n$1 !== undefined ? n$1 : Pervasives.failwith("can not parse integer"),
    amount:
      n$2 !== undefined ? n$2 : Pervasives.failwith("can not parse integer"),
  };
}

var Command = {
  re: re,
  make: make,
};

var $$Array;

var Re;

var MutableStack;

export { $$Array, Re, MutableStack, Stack, Command };
/* No side effect */
