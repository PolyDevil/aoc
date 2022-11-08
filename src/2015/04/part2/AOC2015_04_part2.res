open AOC2015_04_part1

module String = Js.String2
module Int = Belt.Int
module MD5 = ReScriptHash.MD5

let make = s => hack(~substring="000000", ~limit=10000000, s)
