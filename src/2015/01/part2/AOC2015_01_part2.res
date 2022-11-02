module String = Js.String2

let make = s => {
  let v = ref(0)
  let i = ref(0)
  let max = s->String.length
  let break = ref(false)

  while !break.contents {
    v.contents =
      v.contents +
      switch s->String.get(i.contents) {
      | "(" => 1
      | ")" => -1
      | _ => 0
      }

    switch v.contents {
    | -1 => {
        i := i.contents + 1
        v.contents = i.contents
        break := true
      }

    | _ =>
      switch i.contents + 1 === max {
      | true => break := true
      | false => i := i.contents + 1
      }
    }
  }

  v.contents
}
