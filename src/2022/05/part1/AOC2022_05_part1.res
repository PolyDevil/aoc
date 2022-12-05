open AOC2022_05

let make = (state, commands) => {
  let map = state->Array.map(list => list->Stack.init)

  commands->Array.forEach(cmd => {
    let {amount, from, to} = cmd->Command.make
    let from = map->Array.unsafe_get(from - 1)
    let to = map->Array.unsafe_get(to - 1)

    for _ in 1 to amount {
      switch from->Stack.pop {
      | Some(n) => to->Stack.push(n)
      | None => failwith("nothing to pop")
      }
    }
  })

  map->Array.reduce((p, c) =>
    p ++
    switch c->Stack.top {
    | Some(s) => s->Stack.Char.decode
    | None => ""
    }
  , "")
}
