type conj

type disj

val ctrue : conj
val dtrue : disj
val dfalse : disj

module Monad :
sig
  type t = conj -> disj

  val (>>=) : disj -> (conj -> disj) -> disj

  val (>=>) : t -> t -> t
end

module LowLevel :
sig
  val feat : Var.t -> Feat.t -> Var.t -> conj -> disj
  val nfeat : Var.t -> Feat.t -> Var.t -> conj -> disj

  val abs : Var.t -> Feat.t -> conj -> disj
  val nabs : Var.t -> Feat.t -> conj -> disj

  val reg : Var.t -> conj -> disj
  val nreg : Var.t -> conj -> disj

  val dir : Var.t -> conj -> disj
  val ndir : Var.t -> conj -> disj

  val fen : Var.t -> Feat.Set.t -> conj -> disj
  val nfen : Var.t -> Feat.Set.t -> conj -> disj

  val sim : Var.t -> Feat.Set.t -> Var.t -> conj -> disj
  val nsim : Var.t -> Feat.Set.t -> Var.t -> conj -> disj

  val empty : Var.t -> conj -> disj
  val nempty : Var.t -> conj -> disj

  val sim1 : Var.t -> Feat.t -> conj -> disj
end

module WithPath :
sig
  val abs : (Var.t * Path.t) -> Feat.t -> conj -> disj
  val nabs : (Var.t * Path.t) -> Feat.t -> conj -> disj

  val reg : (Var.t * Path.t) -> conj -> disj
  val nreg : (Var.t * Path.t) -> conj -> disj

  val dir : (Var.t * Path.t) -> conj -> disj
  val ndir : (Var.t * Path.t) -> conj -> disj

  val fen : (Var.t * Path.t) -> Feat.Set.t -> conj -> disj
  val nfen : (Var.t * Path.t) -> Feat.Set.t -> conj -> disj

  val empty : (Var.t * Path.t) -> conj -> disj
  val nempty : (Var.t * Path.t) -> conj -> disj

  val sim1 : (Var.t * Path.t) -> conj -> disj
end
