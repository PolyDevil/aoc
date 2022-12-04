module Array = Js.Array2

let hasIntersection = ((s1, e1), (s2, e2)) =>
  switch s1 === s2 {
  | true => true
  | false =>
    switch s1 > s2 {
    | true => s1 <= e2
    | false => e1 >= s2
    }
  }

let make = a => a->Array.reduce((p, (a, b)) =>
    switch hasIntersection(a, b) {
    | true => p + 1
    | false => p
    }
  , 0)
