module Array = Js.Array2

module Game = {
  type input = string

  type state =
    | Rock
    | Paper
    | Scissors

  let toState = (input: input) =>
    switch input {
    | "A" | "X" => Rock
    | "B" | "Y" => Paper
    | "C" | "Z" => Scissors
    | _ => failwith("incorrect value")
    }

  let score = state =>
    switch state {
    | Rock => 1
    | Paper => 2
    | Scissors => 3
    }

  type t =
    | Win(state)
    | Draw(state)
    | Loss(state)

  let play = (s1, s2) =>
    switch (s1, s2) {
    | (Rock, Rock) => Rock->Draw
    | (Rock, Paper) => Paper->Win
    | (Rock, Scissors) => Scissors->Loss
    | (Paper, Rock) => Rock->Loss
    | (Paper, Paper) => Paper->Draw
    | (Paper, Scissors) => Scissors->Win
    | (Scissors, Rock) => Rock->Win
    | (Scissors, Paper) => Paper->Loss
    | (Scissors, Scissors) => Scissors->Draw
    }

  let outcome = t =>
    switch t {
    | Win(state) => 6 + state->score
    | Draw(state) => 3 + state->score
    | Loss(state) => 0 + state->score
    }

  let make = (s1: input, s2: input) => play(s1->toState, s2->toState)->outcome
}

type t = array<(string, string)>

let make = t => t->Array.reduce((prev, (s1, s2)) => prev + Game.make(s1, s2), 0)
