open Constraints_common

module type S = sig
  type t

  val true_ : t

  val eq : Var.t -> Var.t -> t -> t list
  val neq : Var.t -> Var.t -> t -> t list

  val feat : Var.t -> Feat.t -> Var.t -> t -> t list
  val nfeat : Var.t -> Feat.t -> Var.t -> t -> t list

  val abs : Var.t -> Feat.t -> t -> t list
  val nabs : Var.t -> Feat.t -> t -> t list

  val reg : Var.t -> t -> t list
  val nreg : Var.t -> t -> t list

  val dir : Var.t -> t -> t list
  val ndir : Var.t -> t -> t list

  val fen : Var.t -> Feat.Set.t -> t -> t list
  val nfen : Var.t -> Feat.Set.t -> t -> t list

  val sim : Var.t -> Feat.Set.t -> Var.t -> t -> t list
  val nsim : Var.t -> Feat.Set.t -> Var.t -> t -> t list

  val quantify_over : Var.t -> t -> t
end

module Dummy = (Dummy : S)
module Naive = (Naive : S)
module Efficient = (Efficient : S)
