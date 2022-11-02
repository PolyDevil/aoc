let {min_int: min} = module(Js.Math)

let make = (~l, ~w, ~h) => {
  let a = l * w
  let b = l * h
  let c = w * h
  let min = a->min(b)->min(c)

  2 * (a + b + c) + min
}
