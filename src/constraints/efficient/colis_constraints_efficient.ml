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

let exists ?hint f = fun c ->
  let x = Var.fresh ?hint () in
  c |> f x |> List.map (Core.quantify_over x)

let exists2 ?hint1 ?hint2 f =
  exists ?hint:hint1 @@ fun x ->
  exists ?hint:hint2 @@ fun y ->
  f x y

let exists3 ?hint1 ?hint2 ?hint3 f =
  exists ?hint:hint1 @@ fun x ->
  exists ?hint:hint2 @@ fun y ->
  exists ?hint:hint3 @@ fun z ->
  f x y z

let and_ r1 r2 = fun c ->
  c |> r1 |> List.map r2 |> List.flatten

let (&) = and_

let add_to_sat_conj = (@@)

let or_ r1 r2 = fun c ->
  (c |> r1) @ (c |> r2)
