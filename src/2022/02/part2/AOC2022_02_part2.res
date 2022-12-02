module Array = Js.Array2

module Game = {
  open AOC2022_02_part1.Game

  let calculateMove = (s1: input, s2: input) =>
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

  let make = (s1: input, s2: input) => calculateMove(s1, s2)->outcome
}

type t = array<(string, string)>

let make = t => t->Array.reduce((prev, (s1, s2)) => prev + Game.make(s1, s2), 0)
