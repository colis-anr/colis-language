open Colis_constraints_common
open Colis_constraints_efficient_clause
include External

type sat_conj = Core.t                                       [@@deriving yojson]
let true_sat_conj = Core.empty

let pp_sat_conj = PrettyPrinter.pp
let pp_sat_conj_as_dot = PrettyPrinter.pp_as_dot

type t = sat_conj -> sat_conj Dnf.t
let true_ = Dnf.single

let  sim1 x f y = sim x (Feat.Set.singleton f) y

let quantify_over x c =
  Core.quantify_over x c |> Dnf.single

let simplify = Core.simplify

let quantify_over_and_simplify x c =
  Core.(c |> quantify_over x |> simplify) |> Dnf.single

let with_shadow_variables = Core.with_shadow_variables

let exists f = fun c ->
  let x = Var.fresh () in
  c |> f x |> List.map (Core.quantify_over x)

let exists2 f = exists  @@ fun x1 -> exists @@ fun x2 -> f x1 x2
let exists3 f = exists2 @@ fun x1 x2 -> exists @@ fun x3 -> f x1 x2 x3
let exists4 f = exists3 @@ fun x1 x2 x3 -> exists @@ fun x4 -> f x1 x2 x3 x4
let exists5 f = exists4 @@ fun x1 x2 x3 x4 -> exists @@ fun x5 -> f x1 x2 x3 x4 x5
let exists6 f = exists5 @@ fun x1 x2 x3 x4 x5 -> exists @@ fun x6 -> f x1 x2 x3 x4 x5 x6
let exists7 f = exists6 @@ fun x1 x2 x3 x4 x5 x6 -> exists @@ fun x7 -> f x1 x2 x3 x4 x5 x6 x7
let exists8 f = exists7 @@ fun x1 x2 x3 x4 x5 x6 x7 -> exists @@ fun x8 -> f x1 x2 x3 x4 x5 x6 x7 x8
let exists9 f = exists8 @@ fun x1 x2 x3 x4 x5 x6 x7 x8 -> exists @@ fun x9 -> f x1 x2 x3 x4 x5 x6 x7 x8 x9

let and_ r1 r2 = fun c ->
  c |> r1 |> List.map r2 |> List.flatten

let (&) = and_

let and_l = function
  | [] -> true_
  | r :: rs -> List.fold_left and_ r rs

let add_to_sat_conj = (@@)

let or_ r1 r2 = fun c ->
  (c |> r1) @ (c |> r2)

let or_l = function
  | [] -> fun _ -> []
  | r :: rs -> List.fold_left or_ r rs
