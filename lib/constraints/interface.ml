open Constraints_common

module type S = sig
  type conj
  (** Abstract type for satisfiable conjunctive clauses. *)

  type disj
  (** Abstract type for disjunctions of {!conj}. *)

  val ctrue : conj
  val dtrue : disj

  val fold :  ('a -> conj -> 'a) -> 'a -> disj -> 'a

  (** {2 Raw Clauses} *)

  type raw
  (** Abstract type for conjunctive clauses. *)

  val rtrue : raw

  val rfalse : raw

  val exists : (Var.t -> raw) -> raw

  val (&) : raw -> raw -> raw
  (** The conjunction of two [raw]. *)

  val (++) : raw -> raw -> raw
  (** The disjunction of two [raw]. Beware! [(++)] has a higher
     precedence than [(&)]. *)

  val add_to_conj : raw -> conj -> disj

  type term = Var.t * Path.t

  val ex : term -> raw
  (** [ex x\[p\]] means "the path [p] exists in [x]." *)

  val eq : term -> term -> raw
  (** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x]
     and [y] resp. and what's there on both sides is equal." *)

  val neq : term -> term -> raw
  (** [ex_neq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in
     [x] and [y] resp. and what's there on both sides is different."
     *)

  val abs : term -> Feat.t -> raw
  (** [abs x\[p\] f] means "the path [p] exists in [x] and what's
     there does not have the feature [f]." *)

  val reg : term -> raw
  (** [reg x\[p\]] means "the path [p] exists in [x] and what's there
     is a regular file." *)

  val nreg : term -> raw
  (** [nreg x\[p\]] means "the path [p] exists in [x] and what's there
     is not a regular file." *)

  val nex_nreg : term -> raw
  (** [nex_nreg x\[p\]] means "the path [p] does not exist in [x] or
     exists but is not a regular file." *)

  val dir : term -> raw
  (** [dir x\[p\]] means "the path [p] exists in [x] and what's there
     is a directory." *)

  val ndir : term -> raw
  (** [ndir x\[p\]] means "the path [p] exists in [x] and what's there
     isn't a directory." *)

  val nex_ndir : term -> raw
  (** [nex_ndir x\[p\]] means "the path [p] does not exists in [x] or
     exists but is not a directory." *)

  val empty : term -> raw
  (** [empty x\[p\]] means "the path [p] exists in [x] and what's
     there does not have any feature." *)

  val nempty : term -> raw
  (** [nempty x\[p\]] means "the path [p] exists in [x] and what's
     there has a feature." *)

  val sim1 : Var.t -> Path.t -> Var.t -> raw
  (** [sim1 x p y] means, with [p = q/f] "the path [q] exists in both
     [x] and [y] and [x] and [y] may only differ in the name [p]." *)

  val sim2 : Var.t -> Path.t -> Path.t -> Var.t -> raw
(** [sim2 x p1 p2 y] means, with [p1 = q1/f1] and [p2 = q2/f2] "the
   pathes [q1] and [q2] exist in both [x] and [y] and [x] and [y] may
   only differ in the names [p1] and [p2]." *)
end

module Make (I : Constraints_implementation.S) : S = struct
  type conj = I.t
  type disj = conj list

  let ctrue = I.true_
  let dtrue = [ctrue]

  let fold = List.fold_left

  type raw = conj -> disj

  let rtrue = fun c -> [c]
  let rfalse = fun _c -> []

  let exists f = fun c ->
    let x = Var.fresh () in
    c |> f x |> List.map (I.quantify_over x)

  let exists2 f =
    exists @@ fun x ->
              exists @@ fun y ->
                        f x y

  let (&) r1 r2 = fun c ->
    c |> r1 |> List.map r2 |> List.flatten

  let add_to_conj = (@@)

  let (++) r1 r2 = fun c ->
    (c |> r1) @ (c |> r2)

  type term = Var.t * Path.t

  let resolve ((x, p) : term) (z : Var.t) : raw =
    let rec aux x = function
      | [] -> I.eq x z
      | f :: q ->
         exists @@ fun y ->
                   I.feat x f y & aux y q
    in
    aux x (Path.to_list p)

  let noresolve ((x, p) : term) : raw =
    let rec aux x = function
      | [] -> rfalse
      | f :: q ->
         I.abs x f
         ++ (exists @@ fun y ->
                       I.feat x f y & aux y q)
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
               resolve t x & resolve u y & I.neq x y

  let abs t f =
    exists @@ fun x ->
              resolve t x & I.abs x f

  let reg t =
    exists @@ fun x ->
              resolve t x & I.reg x

  let nreg t =
    exists @@ fun x ->
              resolve t x & I.nreg x

  let nex_nreg t =
    noresolve t
    ++ (exists @@ fun x ->
                  resolve t x & I.nreg x)

  let dir t =
    exists @@ fun x ->
              resolve t x & I.dir x

  let ndir t =
    exists @@ fun x ->
              resolve t x & I.ndir x

  let nex_ndir t =
    noresolve t
    ++ (exists @@ fun x ->
                  resolve t x & I.ndir x)

  let empty t =
    exists @@ fun x ->
              resolve t x & I.fen x Feat.Set.empty

  let nempty t =
    exists @@ fun x ->
              resolve t x & I.nfen x Feat.Set.empty

  let sim1 (x : Var.t) (p : Path.t) (y : Var.t) : raw =
    let rec sim1 x p y =
      match p with
      | [] -> rtrue
      | [f] ->
         I.sim x (Feat.Set.singleton f) y
      | f :: q ->
         exists2 @@ fun x' y' ->
                    I.sim x (Feat.Set.singleton f) y
                    & I.feat x f x' & I.feat y f y'
                    & sim1 x' q y'
    in
    sim1 x (Path.to_list p) y

  let sim2 x p q y =
    exists @@ fun z ->
              sim1 x p z & sim1 z q y
end
