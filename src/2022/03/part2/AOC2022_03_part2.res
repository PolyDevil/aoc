module String = Js.String2
module Array = Js.Array2
module Set = Belt.MutableSet.Int
module CodePoint = AOC2022_03_part1.CodePoint

let toSet = s => s->Js.String.castToArrayLike->Array.from->Array.reduce((p, c) =>
    switch c->String.codePointAt(0) {
    | Some(n) => {
        let _ = p->Array.push(n)
        p
      }

    | None => p
    }
  , [])->Set.fromArray

let getPriority = (s0, s1, s2) =>
  switch s0->toSet->Set.intersect(s1->toSet)->Set.intersect(s2->toSet)->Set.toArray {
  | [r] => r
  | _ => failwith("no intersection")
  }->CodePoint.make

type t = array<(string, string, string)>

let make = a => a->Array.reduce((p, (s0, s1, s2)) => p + getPriority(s0, s1, s2), 0)
