let make = (~l, ~w, ~h) =>
  l * w * h +
    switch l > w {
    | true =>
      switch l > h {
      | true => w + h
      | false => w + l
      }
    | false =>
      switch w > h {
      | true => h + l
      | false => w + l
      }
    } * 2
