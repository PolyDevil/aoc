module Array = Js.Array2

module Game = {
  type input = string

  type state =
    | Rock
    | Paper
    | Scissors

  let toState = (input: input) =>
    switch input {
    | "A" => Rock
    | "B" => Paper
    | "C" => Scissors
    | _ => failwith("incorrect value")
    }

  type t =
    | Win(state)
    | Draw(state)
    | Loss(state)

  let decode = (s1: input, s2: input) =>
    switch s2 {
    | "X" =>
      switch s1->toState {
      | Rock => Scissors
      | Paper => Rock
      | Scissors => Paper
      }->Loss
    | "Y" => s1->toState->Draw
    | "Z" =>
      switch s1->toState {
      | Rock => Paper
      | Paper => Scissors
      | Scissors => Rock
      }->Win
    | _ => failwith("incorrect value")
    }

  let score = state =>
    switch state {
    | Rock => 1
    | Paper => 2
    | Scissors => 3
    }

  let outcome = t =>
    switch t {
    | Win(state) => 6 + state->score
    | Draw(state) => 3 + state->score
    | Loss(state) => 0 + state->score
    }

  let make = (s1: input, s2: input) => decode(s1, s2)->outcome
}

type t = array<(string, string)>

let make = t => t->Array.reduce((prev, (s1, s2)) => prev + Game.make(s1, s2), 0)
