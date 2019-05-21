open Constraints_common

module type S = sig
  type t

  val true_ : t

  val  eq : Var.t -> Var.t -> t -> t list
  val neq : Var.t -> Var.t -> t -> t list
  val  feat : Var.t -> Feat.t -> Var.t -> t -> t list
  val nfeat : Var.t -> Feat.t -> Var.t -> t -> t list
  val  abs : Var.t -> Feat.t -> t -> t list
  val nabs : Var.t -> Feat.t -> t -> t list
  val  fen : Var.t -> Feat.Set.t -> t -> t list
  val nfen : Var.t -> Feat.Set.t -> t -> t list
  val  sim : Var.t -> Feat.Set.t -> Var.t -> t -> t list
  val nsim : Var.t -> Feat.Set.t -> Var.t -> t -> t list

  val  kind : Kind.t -> Var.t -> t -> t list
  val nkind : Kind.t -> Var.t -> t -> t list

  val quantify_over : Var.t -> t -> t list

  val pp : Format.formatter -> t -> unit
  val pp_as_dot : name:string -> Format.formatter -> t -> unit
end

module Dummy = (Dummy : S)
module Naive = (Constraints_implementation_naive : S)
module Efficient = (Constraints_implementation_efficient : S)
