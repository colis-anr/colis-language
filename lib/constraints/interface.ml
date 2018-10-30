open Constraints_common
module E = Constraints_implementation.Efficient
module L = ListMonad

type conj = unit
type disj = conj list

let ctrue = ()
let dtrue = [ctrue]

let fold = List.fold_left

type raw = conj -> disj

let rtrue = L.singleton
let rfalse = fun _c -> []

let exists f = fun c ->
  let x = Var.fresh () in
  c |> f x |> List.map (E.quantify_over x)

let exists2 f =
  exists @@ fun x ->
  exists @@ fun y ->
  f x y

let (&) = L.(>=>)
let add_to_conj = (@@)

let (++) r1 r2 = fun c ->
  (c |> r1) @ (c |> r2)

type term = Var.t * Path.t

let resolve ((x, p) : term) (z : Var.t) : raw =
  let rec aux x = function
    | [] -> E.eq x z
    | f :: q ->
       exists @@ fun y ->
       E.feat x f y & aux y q
  in
  aux x (Path.to_list p)

let noresolve ((x, p) : term) : raw =
  let rec aux x = function
    | [] -> rfalse
    | f :: q ->
       E.abs x f
       ++ (exists @@ fun y ->
           E.feat x f y & aux y q)
  in
  aux x (Path.to_list p)

let ex t =
  exists @@ fun x ->
  resolve t x

let eq t u =
  exists @@ fun z ->
  resolve t z & resolve u z

let neq t u =
  exists2 @@ fun x y ->
  resolve t x & resolve u y & E.neq x y

let abs t f =
  exists @@ fun x ->
  resolve t x & E.abs x f

let reg t =
  exists @@ fun x ->
  resolve t x & E.reg x

let nreg t =
  exists @@ fun x ->
  resolve t x & E.nreg x

let nex_nreg t =
  noresolve t
  ++ (exists @@ fun x ->
      resolve t x & E.nreg x)

let dir t =
  exists @@ fun x ->
  resolve t x & E.dir x

let ndir t =
  exists @@ fun x ->
  resolve t x & E.ndir x

let nex_ndir t =
  noresolve t
  ++ (exists @@ fun x ->
      resolve t x & E.ndir x)

let empty t =
  exists @@ fun x ->
  resolve t x & E.fen x Feat.Set.empty

let nempty t =
  exists @@ fun x ->
  resolve t x & E.nfen x Feat.Set.empty

let sim1 (x : Var.t) (p : Path.t) (y : Var.t) : raw =
  let rec sim1 x p y =
    match p with
    | [] -> rtrue
    | [f] ->
       E.sim x (Feat.Set.singleton f) y
    | f :: q ->
       exists2 @@ fun x' y' ->
       E.sim x (Feat.Set.singleton f) y
       & E.feat x f x' & E.feat y f y'
       & sim1 x' q y'
  in
  sim1 x (Path.to_list p) y

let sim2 x p q y =
  exists @@ fun z ->
  sim1 x p z & sim1 z q y
