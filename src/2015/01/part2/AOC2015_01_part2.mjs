// Generated by ReScript, PLEASE EDIT WITH CARE


function make(s) {
  var v = 0;
  var i = 0;
  var max = s.length;
  var $$break = false;
  while(!$$break) {
    var match = s[i];
    var tmp;
    switch (match) {
      case "(" :
          tmp = 1;
          break;
      case ")" :
          tmp = -1;
          break;
      default:
        tmp = 0;
    }
    v = v + tmp | 0;
    var match$1 = v;
    if (match$1 !== -1) {
      if ((i + 1 | 0) === max) {
        $$break = true;
      } else {
        i = i + 1 | 0;
      }
    } else {
      i = i + 1 | 0;
      v = i;
      $$break = true;
    }
  };
  return v;
}

var $$String;

export {
  $$String ,
  make ,
}
/* No side effect */
