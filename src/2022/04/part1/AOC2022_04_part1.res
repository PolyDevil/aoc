module Array = Js.Array2

let isInRange = ((s1, e1), (s2, e2)) => (s1 <= s2 && e2 <= e1) || (s1 >= s2 && e2 >= e1)

let make = a => a->Array.reduce((p, (a, b)) =>
    switch isInRange(a, b) {
    | true => p + 1
    | false => p
    }
  , 0)
