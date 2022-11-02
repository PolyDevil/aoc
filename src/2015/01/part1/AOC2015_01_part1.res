module String = Js.String2

let make = s => {
  let v = ref(0)

  for i in 0 to s->String.length - 1 {
    v.contents =
      v.contents +
      switch s->String.get(i) {
      | "(" => 1
      | ")" => -1
      | _ => 0
      }
  }

  v.contents
}
