// Generated by ReScript, PLEASE EDIT WITH CARE

import * as AOC2015_01_part2 from "./AOC2015_01_part2.mjs";

var cases = [
  [
    ")",
    1
  ],
  [
    "()())",
    5
  ]
];

function make(param) {
  return cases.every(function (param) {
              return AOC2015_01_part2.make(param[0]) === param[1];
            });
}

export {
  cases ,
  make ,
}
/* No side effect */
