module String = Js.String2
module Int = Belt.Int
module MD5 = ReScriptHash.MD5

let hack = (~substring, ~limit, s) => {
  let i = ref(0)
  let v = ref(None)
  let break = ref(false)

  while !break.contents {
    switch i.contents === limit {
    | true => break := true
    | false => {
        let hash = MD5.makeU(. s ++ i.contents->Int.toString)
        switch hash->String.startsWith(substring) {
        | true => {
            v.contents = Some(i.contents)
            break := true
          }

        | false => i := i.contents + 1
        }
      }
    }
  }

  v.contents
}

let make = s => hack(~substring="00000", ~limit=10000000, s)
