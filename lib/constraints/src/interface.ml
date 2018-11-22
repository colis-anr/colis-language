open Constraints_common

module type S = sig
  (** {2 Feature Formulaes} *)

  type t
  (** Abstract type for conjunctive clauses. *)

  val and_ : t -> t -> t
  (** The conjunction of two [t]. *)

  val (&) : t -> t -> t
  (** The conjunction of two [t] (alias for {!and}). *)

  val or_ : t -> t -> t
  (** The disjunction of two [t]. *)

  val var : (Var.t -> t) -> t
  val var2 : (Var.t -> Var.t -> t) -> t

  val exists : (Var.t -> t) -> t
  val exists2 : (Var.t -> Var.t -> t) -> t

  val eq : Var.t -> Var.t -> t
  val neq : Var.t -> Var.t -> t
  val feat : Var.t -> Feat.t -> Var.t -> t
  val nfeat : Var.t -> Feat.t -> Var.t -> t
  val abs : Var.t -> Feat.t -> t
  val nabs : Var.t -> Feat.t -> t
  val reg : Var.t -> t
  val nreg : Var.t -> t
  val dir : Var.t -> t
  val ndir : Var.t -> t
  val fen : Var.t -> Feat.Set.t -> t
  val nfen : Var.t -> Feat.Set.t -> t
  val empty : Var.t -> t
  val nempty : Var.t -> t
  val sim : Var.t -> Feat.Set.t -> Var.t -> t
  val nsim : Var.t -> Feat.Set.t -> Var.t -> t
  val sim1 : Var.t -> Feat.t -> Var.t -> t
  val nsim1 : Var.t -> Feat.t -> Var.t -> t
  val sim2 : Var.t -> Feat.t -> Feat.t -> Var.t -> t
  val nsim2 : Var.t -> Feat.t -> Feat.t -> Var.t -> t

  (** {2 Satisfiable clauses} *)

  type sat_conj
  (** Abstract type for satisfiable conjunctions. *)

  val true_ : sat_conj
  (** The empty conjunction, true. *)

  val add_to_sat_conj : t -> sat_conj -> sat_conj list
  (** [add_to_sat_conj f c] adds the formula [f] to a satisfiable
     conjunction [c]. The result is a list of satisfiable conjunctions
     whose disjunction is equivalent to ([c] &and; [f]). The list
     might be empty when ([c] &and; [f]) is unsatisfiable. *)
end

module Make (I : Constraints_implementation.S) : S = struct
  type sat_conj = I.t
  let true_ = I.true_

  type t = sat_conj -> sat_conj list

  let eq = I.eq
  let neq = I.neq
  let feat = I.feat
  let nfeat = I.nfeat
  let abs = I.abs
  let nabs = I.nabs
  let reg = I.reg
  let nreg = I.nreg
  let dir = I.dir
  let ndir = I.ndir
  let fen = I.fen
  let nfen = I.nfen
  let sim = I.sim
  let nsim = I.nsim

  let empty x = fen x Feat.Set.empty
  let nempty x = nfen x Feat.Set.empty
  let sim1 x f y = sim x (Feat.Set.singleton f) y
  let nsim1 x f y = sim x (Feat.Set.singleton f) y
  let sim2 x f g y = sim x Feat.Set.(add f (singleton g)) y
  let nsim2 x f g y = sim x Feat.Set.(add f (singleton g)) y

  let var f = fun c ->
    let x = Var.fresh () in
    c |> f x

  let var2 f =
    var @@ fun x ->
    var @@ fun y ->
    f x y

  let exists f = fun c ->
    let x = Var.fresh () in
    c |> f x |> List.map (I.quantify_over x) |> List.flatten

  let exists2 f =
    exists @@ fun x ->
    exists @@ fun y ->
    f x y

  let and_ r1 r2 = fun c ->
    c |> r1 |> List.map r2 |> List.flatten

  let (&) = and_

  let add_to_sat_conj = (@@)

  let or_ r1 r2 = fun c ->
    (c |> r1) @ (c |> r2)
end
