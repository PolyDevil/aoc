open AOC2022_05

let make = (state, commands) => {
  let map = state->Array.map(list => list->Stack.init)

  commands->Array.forEach(cmd => {
    let {amount, from, to} = cmd->Command.make
    let from = map->Array.unsafe_get(from - 1)
    let to = map->Array.unsafe_get(to - 1)

    let cache = []

    for _ in 1 to amount {
      let _ = switch from->Stack.pop {
      | Some(n) => cache->Array.push(n)
      | None => failwith("nothing to pop")
      }
    }

    let cache = cache->Array.reverseInPlace

    cache->Array.forEach(e => to->Stack.push(e))
  })

  map->Array.reduce((p, c) =>
    p ++
    switch c->Stack.top {
    | Some(s) => s->Stack.Char.decode
    | None => ""
    }
  , "")
}
