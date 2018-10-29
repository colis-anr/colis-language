type 'a t = 'a list

let singleton x = [x]

let bind (l : 'a t) (f : 'a -> 'b t) : 'b t =
  List.map f l |> List.flatten

let (>>=) = bind

let compose (f1 : 'a -> 'b t) (f2 : 'b -> 'c t) : 'a -> 'c t = fun x ->
  singleton x >>= f1 >>= f2

let (>=>) = compose
