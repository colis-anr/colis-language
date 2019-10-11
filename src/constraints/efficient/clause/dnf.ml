type 'a t = 'a list

let empty = []
let single x = [x]

let bind x f =
  List.map f x |> List.flatten

let compose f g =
  fun x ->
  bind (f x) g

module Syntax = struct
  let (>>=) = bind
  let (=<<) f x = bind x f
  let (>=>) = compose
end
