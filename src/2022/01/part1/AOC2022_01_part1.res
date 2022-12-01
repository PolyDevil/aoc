module Array = Js.Array2

let {max_int: max} = module(Js.Math)

type t = array<array<int>>

let make = t =>
  t->Array.reduce((prev, list) => max(prev, list->Array.reduce((p, c) => p + c, 0)), 0)
