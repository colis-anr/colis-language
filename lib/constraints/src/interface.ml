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

  val exists : ?hint:string -> (Var.t -> t) -> t
  val exists2 : ?hint1:string -> ?hint2:string -> (Var.t -> Var.t -> t) -> t
  val exists3 : ?hint1:string -> ?hint2:string -> ?hint3:string -> (Var.t -> Var.t -> Var.t -> t) -> t

  val true_ : t

  val  eq : Var.t -> Var.t -> t
  val neq : Var.t -> Var.t -> t

  val  feat : Var.t -> Feat.t -> Var.t -> t
  val nfeat : Var.t -> Feat.t -> Var.t -> t

  val  abs : Var.t -> Feat.t -> t
  val nabs : Var.t -> Feat.t -> t

  val  maybe : Var.t -> Feat.t -> Var.t -> t
  val nmaybe : Var.t -> Feat.t -> Var.t -> t

  val  fen : Var.t -> Feat.Set.t -> t
  val nfen : Var.t -> Feat.Set.t -> t
  val  empty : Var.t -> t
  val nempty : Var.t -> t

  val  sim : Var.t -> Feat.Set.t -> Var.t -> t
  val nsim : Var.t -> Feat.Set.t -> Var.t -> t
  val  sim1 : Var.t -> Feat.t -> Var.t -> t
  val nsim1 : Var.t -> Feat.t -> Var.t -> t
  val  sim2 : Var.t -> Feat.t -> Feat.t -> Var.t -> t
  val nsim2 : Var.t -> Feat.t -> Feat.t -> Var.t -> t

  val  kind : Kind.t -> Var.t -> t
  val nkind : Kind.t -> Var.t -> t

  val  dir : Var.t -> t
  val ndir : Var.t -> t

  val  block : Var.t -> t
  val nblock : Var.t -> t
  val  char : Var.t -> t
  val nchar : Var.t -> t
  val  pipe : Var.t -> t
  val npipe : Var.t -> t
  val  reg : Var.t -> t
  val nreg : Var.t -> t
  val  sock : Var.t -> t
  val nsock : Var.t ->t
  val  symlink : Var.t -> t
  val nsymlink : Var.t -> t

  (** {2 Macros} *)

  val resolve : Var.t -> Path.normal -> Path.t -> Var.t -> t
  val noresolve : Var.t -> Path.normal -> Path.t -> t
  val similar : Var.t -> Var.t -> Path.normal -> Path.t -> Var.t -> Var.t -> t

  (** {2 Satisfiable clauses} *)

  type sat_conj
  (** Abstract type for satisfiable conjunctions. *)

  val true_sat_conj : sat_conj
  (** The empty conjunction, true. *)

  val add_to_sat_conj : t -> sat_conj -> sat_conj list
  (** [add_to_sat_conj f c] adds the formula [f] to a satisfiable
      conjunction [c]. The result is a list of satisfiable conjunctions
      whose disjunction is equivalent to ([c] &and; [f]). The list
      might be empty when ([c] &and; [f]) is unsatisfiable. *)

  val quantify_over : Var.t -> sat_conj -> sat_conj list

  (** {2 Printing} *)

  val make_initial : sat_conj -> sat_conj
  (** [make_initial] does not change the meaning of the conjunction. However, it
      marks all variables as initial. Those variables will revert back to
      non-initial after that if observed. The pretty-printing will then hide
      everything about initial variables. This is useful if one wants to add a
      precondition to cut reach unsatisfiability faster but without poluting the
      printer output when the formula has nothing to do with this precondition.
  *)

  val pp_sat_conj : Format.formatter -> sat_conj -> unit
  val pp_sat_conj_as_dot : name:string -> Format.formatter -> sat_conj -> unit
end

module Make (I : Constraints_implementation.S) : S = struct
  type sat_conj = I.t
  let true_sat_conj = I.true_

  type t = sat_conj -> sat_conj list

  let true_ x = [x]

  let  eq = I.eq
  let neq = I.neq

  let  feat = I.feat
  let nfeat = I.nfeat

  let  abs = I.abs
  let nabs = I.nabs

  let  maybe = I.maybe
  let nmaybe = I.nmaybe

  let  fen = I.fen
  let nfen = I.nfen

  let  sim = I.sim
  let nsim = I.nsim

  let  empty x = fen x Feat.Set.empty
  let nempty x = nfen x Feat.Set.empty
  let  sim1 x f y = sim x (Feat.Set.singleton f) y
  let nsim1 x f y = sim x (Feat.Set.singleton f) y
  let  sim2 x f g y = sim x Feat.Set.(add f (singleton g)) y
  let nsim2 x f g y = sim x Feat.Set.(add f (singleton g)) y

  let kind = I.kind
  let nkind = I.nkind

  let  reg =  kind Kind.Reg
  let nreg = nkind Kind.Reg
  let  dir =  kind Kind.Dir
  let ndir = nkind Kind.Dir
  let  block =  kind Kind.Block
  let nblock = nkind Kind.Block
  let  sock =  kind Kind.Sock
  let nsock = nkind Kind.Sock
  let  pipe =  kind Kind.Pipe
  let npipe = nkind Kind.Pipe
  let  char =  kind Kind.Char
  let nchar = nkind Kind.Char
  let  symlink =  kind Kind.Symlink
  let nsymlink = nkind Kind.Symlink

  let exists ?hint f = fun c ->
    let x = Var.fresh ?hint () in
    c |> f x |> List.map (I.quantify_over x) |> List.flatten

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

  let rec resolve_stack x pi q z =
    match Path.split_first_rel q with
    | None -> eq x z
    | Some (Down f, q) ->
      exists ~hint:(Feat.to_string f) @@ fun y ->
      feat x f y & resolve_stack y (x :: pi) q z
    | Some (Here, q) ->
      resolve_stack x pi q z
    | Some (Up, q) ->
      match pi with
      | [] -> resolve_stack x [] q z
      | y::pi -> dir x & resolve_stack y pi q z

  let resolve r cwd q z =
    resolve_stack r [] (Path.concat cwd q) z

  let rec noresolve_stack x pi q =
    match Path.split_first_rel q with
    | None -> (fun _ -> []) (* false *)
    | Some (Down f, q) ->
      (
        match Path.split_first_rel q with
        | None ->
          abs x f
        | _ ->
          exists ~hint:(Feat.to_string f) @@ fun y ->
          maybe x f y & noresolve_stack y (x::pi) q
      )
    | Some (Here, q) ->
      noresolve_stack x pi q
    | Some (Up, q) ->
      match pi with
      | [] -> noresolve_stack x [] q
      | y::pi -> or_ (ndir x) (noresolve_stack y pi q)

  let noresolve r cwd q =
    dir r & noresolve_stack r [] (Path.concat cwd q)

  let rec similar_normal x x' p z z' =
    match p with
    | [] ->
      eq x z & eq x' z'
    | f::p ->
      exists2 @@ fun y y' ->
      feat x f y & feat x' f y' & sim1 x f x' & similar_normal y y' p z z'

  let similar r r' cwd q z z' =
    similar_normal r r' Path.(normalize ~cwd q) z z'

  let quantify_over = I.quantify_over

  let make_initial = I.make_initial

  let pp_sat_conj = I.pp
  let pp_sat_conj_as_dot = I.pp_as_dot
end
