// Generated by ReScript, PLEASE EDIT WITH CARE

import * as AOC2015_01_part1 from "./AOC2015_01_part1.mjs";

var cases = [
  [
    "(())",
    0
  ],
  [
    "()()",
    0
  ],
  [
    "(((",
    3
  ],
  [
    "(()(()(",
    3
  ],
  [
    "())",
    -1
  ],
  [
    "))(",
    -1
  ],
  [
    ")))",
    -3
  ],
  [
    ")())())",
    -3
  ]
];

function make(param) {
  return cases.every(function (param) {
              return AOC2015_01_part1.make(param[0]) === param[1];
            });
}

export {
  cases ,
  make ,
}
/* No side effect */
