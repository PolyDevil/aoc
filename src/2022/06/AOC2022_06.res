module Map = Belt.MutableMap.String
module String = Js.String2

let make = (size, s) => {
  let i = ref(0)
  let max = s->String.length
  let break = ref(false)
  let map = Map.make()

  while !break.contents {
    if i.contents + 1 === max || map->Map.size === size {
      break := true
    } else {
      let letter = s->String.get(i.contents)

      switch map->Map.has(letter) {
      | true =>
        switch map->Map.get(letter) {
        | Some(v) => {
            map->Map.clear
            i := v + 1
          }

        | None => failwith("")
        }
      | false => {
          map->Map.set(letter, i.contents)
          i := i.contents + 1
        }
      }
    }
  }

  i.contents
}
