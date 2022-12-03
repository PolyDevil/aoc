// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Belt_MutableSetInt from "rescript/lib/es6/belt_MutableSetInt.js";

function make(n) {
  if (n >= 97) {
    return n - 96 | 0;
  } else {
    return n - 38 | 0;
  }
}

var CodePoint = {
  make: make
};

function getPriority(s) {
  var i = 0;
  var r;
  var mid = s.length / 2 | 0;
  var $$break = false;
  var s0 = Belt_MutableSetInt.make(undefined);
  var s1 = Belt_MutableSetInt.make(undefined);
  while(!$$break) {
    var match = s[i].codePointAt(0);
    var match$1 = s[i + mid | 0].codePointAt(0);
    if (match !== undefined && match$1 !== undefined) {
      if (match === match$1 || Belt_MutableSetInt.has(s0, match$1)) {
        r = match$1;
        $$break = true;
      } else {
        Belt_MutableSetInt.add(s0, match);
        Belt_MutableSetInt.add(s1, match$1);
        if ((i + 1 | 0) === mid) {
          $$break = true;
        } else {
          i = i + 1 | 0;
        }
      }
    } else {
      $$break = true;
    }
  };
  var r$1 = r;
  var tmp;
  if (r$1 !== undefined) {
    tmp = r$1;
  } else {
    var match$2 = Belt_MutableSetInt.toArray(Belt_MutableSetInt.intersect(s0, s1));
    tmp = match$2.length !== 1 ? Pervasives.failwith("no intersection") : match$2[0];
  }
  return make(tmp);
}

function make$1(a) {
  return a.reduce((function (p, c) {
                return p + getPriority(c) | 0;
              }), 0);
}

var $$String;

var $$Array;

var $$Set;

export {
  $$String ,
  $$Array ,
  $$Set ,
  CodePoint ,
  getPriority ,
  make$1 as make,
}
/* No side effect */
