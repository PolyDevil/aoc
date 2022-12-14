// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as AOC2022_05 from "../AOC2022_05.mjs";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function make(state, commands) {
  var map = state.map(function (list) {
    return AOC2022_05.Stack.init(list);
  });
  commands.forEach(function (cmd) {
    var match = AOC2022_05.Command.make(cmd);
    var from = map[(match.from - 1) | 0];
    var to = map[(match.to - 1) | 0];
    for (
      var _for = 1, _for_finish = match.amount;
      _for <= _for_finish;
      ++_for
    ) {
      var n = Curry._1(AOC2022_05.Stack.pop, from);
      if (n !== undefined) {
        Curry._2(AOC2022_05.Stack.push, to, n);
      } else {
        Pervasives.failwith("nothing to pop");
      }
    }
  });
  return map.reduce(function (p, c) {
    var s = Curry._1(AOC2022_05.Stack.top, c);
    return (
      p + (s !== undefined ? Curry._1(AOC2022_05.Stack.Char.decode, s) : "")
    );
  }, "");
}

export { make };
/* No side effect */
