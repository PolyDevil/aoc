module Array = Js.Array2
module Re = Js.Re
module String = Js.String2

let make = s =>
  s->Re.test_(%re(`/^((?!ab|cd|pq|xy).)*$/g`), _) &&
    switch s->String.match_(%re(`/[aeiou]/g`)) {
    | Some(matches) => matches->Array.length >= 3 && s->Re.test_(%re(`/(.)\1/g`), _)
    | None => false
    }
