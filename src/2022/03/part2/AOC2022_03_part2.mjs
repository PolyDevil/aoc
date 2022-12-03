// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as AOC2022_03_part1 from "../part1/AOC2022_03_part1.mjs";
import * as Belt_MutableSetInt from "rescript/lib/es6/belt_MutableSetInt.js";

function toSet(s) {
  return Belt_MutableSetInt.fromArray(Array.from(s).reduce((function (p, c) {
                    var n = c.codePointAt(0);
                    if (n !== undefined) {
                      return p.concat([n]);
                    } else {
                      return p;
                    }
                  }), []));
}

function getPriority(s0, s1, s2) {
  var match = Belt_MutableSetInt.toArray(Belt_MutableSetInt.intersect(Belt_MutableSetInt.intersect(toSet(s0), toSet(s1)), toSet(s2)));
  return AOC2022_03_part1.CodePoint.make(match.length !== 1 ? Pervasives.failwith("no intersection") : match[0]);
}

function make(a) {
  return a.reduce((function (p, param) {
                return p + getPriority(param[0], param[1], param[2]) | 0;
              }), 0);
}

var $$String;

var $$Array;

var $$Set;

var CodePoint;

export {
  $$String ,
  $$Array ,
  $$Set ,
  CodePoint ,
  toSet ,
  getPriority ,
  make ,
}
/* No side effect */
