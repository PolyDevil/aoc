// Generated by ReScript, PLEASE EDIT WITH CARE

function make(n) {
  if (n >= 97) {
    return (n - 96) | 0;
  } else {
    return (n - 38) | 0;
  }
}

var CodePoint = {
  make: make,
};

export { CodePoint };
/* No side effect */
