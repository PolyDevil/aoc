module String = Js.String2
module Map = Belt.MutableMap
module SetInt = Belt.MutableSet.Int

module M = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a: t, b: t) => a - b
})

let make = s => {
  let counter = ref(0)
  let cursor = ref((0, 0))
  let path = Map.make(~id=module(M))

  for i in 0 to s->String.length - 1 {
    let (x, y) = cursor.contents

    cursor :=
      switch s->String.get(i) {
      | ">" => (x + 1, y)
      | "v" => (x, y - 1)
      | "<" => (x - 1, y)
      | "^" => (x, y + 1)
      | _ => (x, y)
      }

    let (x, y) = cursor.contents

    switch Map.get(path, x) {
    | Some(v) =>
      switch SetInt.has(v, y) {
      | true => counter := counter.contents + 1
      | false => SetInt.add(v, y)
      }
    | None => Map.set(path, x, SetInt.fromArray([y]))
    }
  }

  s->String.length - counter.contents
}
