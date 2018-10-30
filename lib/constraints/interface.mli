open Constraints_common

(** {1 Clauses} *)

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
(** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x] and
   [y] resp. and what's there on both sides is equal." *)

val neq : term -> term -> raw
(** [ex_neq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x]
   and [y] resp. and what's there on both sides is different." *)

val abs : term -> Feat.t -> raw
(** [abs x\[p\] f] means "the path [p] exists in [x] and what's there
   does not have the feature [f]." *)

val reg : term -> raw
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there is
   a regular file." *)

val nreg : term -> raw
(** [nreg x\[p\]] means "the path [p] exists in [x] and what's there
   is not a regular file." *)

val nex_nreg : term -> raw
(** [nex_nreg x\[p\]] means "the path [p] does not exist in [x] or
   exists but is not a regular file." *)

val dir : term -> raw
(** [dir x\[p\]] means "the path [p] exists in [x] and what's there is
   a directory." *)

val ndir : term -> raw
(** [ndir x\[p\]] means "the path [p] exists in [x] and what's there
   isn't a directory." *)

val nex_ndir : term -> raw
(** [nex_ndir x\[p\]] means "the path [p] does not exists in [x] or
   exists but is not a directory." *)

val empty : term -> raw
(** [empty x\[p\]] means "the path [p] exists in [x] and what's there
   does not have any feature." *)

val nempty : term -> raw
(** [nempty x\[p\]] means "the path [p] exists in [x] and what's there
   has a feature." *)

val sim1 : Var.t -> Path.t -> Var.t -> raw
(** [sim1 x p y] means, with [p = q/f] "the path [q] exists in both
   [x] and [y] and [x] and [y] may only differ in the name [p]." *)

val sim2 : Var.t -> Path.t -> Path.t -> Var.t -> raw
(** [sim2 x p1 p2 y] means, with [p1 = q1/f1] and [p2 = q2/f2] "the
   pathes [q1] and [q2] exist in both [x] and [y] and [x] and [y] may
   only differ in the names [p1] and [p2]." *)
