module CodePoint = {
  let make = n =>
    switch n >= 97 {
    | true => n - 96
    | false => n - 38
    }
}
