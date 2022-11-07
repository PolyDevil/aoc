module String = Js.String2
module Map = Belt.MutableMap
module SetInt = Belt.MutableSet.Int

module M = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a: t, b: t) => a - b
})

module Walker = {
  type t = (int, int)

  type actions = {
    get: unit => t,
    move: string => t,
  }

  let make = () => {
    let coords = ref((0, 0))

    let move = s => {
      let (x, y) = coords.contents

      coords :=
        switch s {
        | ">" => (x + 1, y)
        | "v" => (x, y - 1)
        | "<" => (x - 1, y)
        | "^" => (x, y + 1)
        | _ => (x, y)
        }

      coords.contents
    }

    let get = () => coords.contents

    {
      get,
      move,
    }
  }
}

let make = s => {
  let alreadyVisited = ref(0)
  let santa = Walker.make()
  let roboSanta = Walker.make()
  let path = Map.make(~id=module(M))
  Map.set(path, 0, SetInt.fromArray([0]))

  for i in 0 to s->String.length - 1 {
    let l = s->Js.String2.get(i)

    let (x, y) = switch mod(i, 2) === 0 {
    | false => santa.move(l)
    | true => roboSanta.move(l)
    }

    switch Map.get(path, x) {
    | Some(v) =>
      switch SetInt.has(v, y) {
      | true => alreadyVisited := alreadyVisited.contents + 1
      | false => SetInt.add(v, y)
      }
    | None => Map.set(path, x, SetInt.fromArray([y]))
    }
  }

  s->String.length - alreadyVisited.contents + 1
}
