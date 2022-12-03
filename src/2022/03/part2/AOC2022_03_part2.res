let {castToArrayLike} = module(Js.String)
let {codePointAt} = module(Js.String2)
let {push, reduce, from} = module(Js.Array2)
let {intersect, fromArray, toArray} = module(Belt.MutableSet.Int)
module CodePoint = AOC2022_03.CodePoint

let toSet = s => s->castToArrayLike->from->reduce((p, c) =>
    switch c->codePointAt(0) {
    | Some(n) => {
        let _ = p->push(n)
        p
      }

    | None => p
    }
  , [])->fromArray

let getPriority = (s0, s1, s2) =>
  switch s0->toSet->intersect(s1->toSet)->intersect(s2->toSet)->toArray {
  | [r] => r
  | _ => failwith("no intersection")
  }->CodePoint.make

let make = a => a->reduce((p, (s0, s1, s2)) => p + getPriority(s0, s1, s2), 0)
