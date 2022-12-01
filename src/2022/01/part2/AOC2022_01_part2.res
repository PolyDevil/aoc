module Array = Js.Array2

type t = array<array<int>>

let sum = a => a->Array.reduce((p, c) => p + c, 0)

let make = t =>
  t->Array.map(sum)->Array.sortInPlaceWith((a, b) => b - a)->Array.slice(~start=0, ~end_=3)->sum
