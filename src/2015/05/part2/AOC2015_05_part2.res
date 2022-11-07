module Re = Js.Re
module String = Js.String2

let make = s =>
  s->String.length > 3 &&
  s->Re.test_(%re(`/(.)[^\1]\1/g`), _) &&
  s->Re.test_(%re(`/(.)([^\1])(?:.*\1\2)/g`), _)
