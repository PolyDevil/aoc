module String = Js.String2
module Array = Js.Array2
module Set = Belt.MutableSet.Int

module CodePoint = {
  let make = n =>
    switch n >= 97 {
    | true => n - 96
    | false => n - 38
    }
}

let getPriority = s => {
  let i = ref(0)
  let r = ref(None)
  let mid = s->String.length / 2
  let break = ref(false)
  let s0 = Set.make()
  let s1 = Set.make()

  while !break.contents {
    switch (
      s->String.get(i.contents)->String.codePointAt(0),
      s->String.get(i.contents + mid)->String.codePointAt(0),
    ) {
    | (Some(c0), Some(c1)) =>
      if c0 === c1 {
        r := c1->Some
        break := true
      } else if s0->Set.has(c1) {
        r := c1->Some
        break := true
      } else {
        s0->Set.add(c0)
        s1->Set.add(c1)

        if i.contents + 1 === mid {
          break := true
        } else {
          i := i.contents + 1
        }
      }

    | _ => break := true
    }
  }

  switch r.contents {
  | Some(r) => r
  | None =>
    switch Set.intersect(s0, s1)->Set.toArray {
    | [r] => r
    | _ => failwith("no intersection")
    }
  }->CodePoint.make
}

let make = a => a->Array.reduce((p, c) => p + c->getPriority, 0)
