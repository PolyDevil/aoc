module Array = Js.Array2
module Re = Js.Re
module MutableStack = Belt.MutableStack

module Stack = {
  module Char = {
    let encode = s =>
      switch s->Js.String2.codePointAt(0) {
      | Some(n) => n
      | None => failwith("cant get code point from string")
      }

    let decode = Js.String2.fromCodePoint
  }

  include MutableStack

  let _push = (stack, s) => stack->MutableStack.push(s->Char.encode)
  let populate = (stack, a) => a->Array.forEach(e => stack->_push(e))

  let init = a => {
    let stack = make()
    stack->populate(a)
    stack
  }
}

module Command = {
  type t = {
    from: int,
    to: int,
    amount: int,
  }

  type groups = {
    from: string,
    to: string,
    amount: string,
  }

  type capture = {groups: groups}

  external castToCaptureGroup: Re.result => capture = "%identity"

  let re = %re("/^move\s(?<amount>\d+)\sfrom\s(?<from>\d+)\sto\s(?<to>\d+)$/")

  let make: string => t = s => {
    switch re->Js.Re.exec_(s) {
    | Some(e) => {
        let {groups} = e->castToCaptureGroup

        {
          from: switch groups.from->Belt.Int.fromString {
          | Some(n) => n
          | None => failwith("can not parse integer")
          },
          to: switch groups.to->Belt.Int.fromString {
          | Some(n) => n
          | None => failwith("can not parse integer")
          },
          amount: switch groups.amount->Belt.Int.fromString {
          | Some(n) => n
          | None => failwith("can not parse integer")
          },
        }
      }

    | None => failwith("can not parse command")
    }
  }
}
