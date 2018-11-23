type 'a m = 'a option

let (>>=) x f =
  match x with
  | None -> None
  | Some x -> f x
