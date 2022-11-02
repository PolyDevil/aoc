let {max_int: max, min_int: min} = module(Js.Math)

let make = (~l, ~w, ~h) => l * w * h + (l + w + h - max(l, max(w, h))) * 2
