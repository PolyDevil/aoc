type t = array<(string, int)>

let cases = [(")", 1), ("()())", 5)]

let make = () => cases->Js.Array2.every(((s, i)) => AOC2015_01_part2.make(s) === i)
