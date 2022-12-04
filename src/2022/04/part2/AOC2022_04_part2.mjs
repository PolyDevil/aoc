// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function hasIntersection(param, param$1) {
  var s2 = param$1[0];
  var s1 = param[0];
  if (s1 === s2) {
    return true;
  } else if (Caml_obj.greaterthan(s1, s2)) {
    return Caml_obj.lessequal(s1, param$1[1]);
  } else {
    return Caml_obj.greaterequal(param[1], s2);
  }
}

function make(a) {
  return a.reduce((function (p, param) {
                if (hasIntersection(param[0], param[1])) {
                  return p + 1 | 0;
                } else {
                  return p;
                }
              }), 0);
}

export {
  make ,
}
/* No side effect */
