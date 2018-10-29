(** {1 Clauses} *)

type conj
(** Abstract type for satisfiable conjunctive clauses. *)

type disj
(** Abstract type for disjunctions of {!conj}. *)

val ctrue : conj
val dtrue : disj

val fold :  ('a -> conj -> 'a) -> 'a -> disj -> 'a

(** {2 Raw Conjunctive Clauses} *)

type raw_conj
(** Abstract type for conjunctive clauses. *)

val rtrue : raw_conj

val rfalse : raw_conj

val exists : (Var.t -> raw_conj) -> raw_conj

val (&) : raw_conj -> raw_conj -> raw_conj
(** The conjunction of two [raw_conj]. *)

val (++) : raw_conj -> raw_conj -> raw_conj
(** The disjunction of two [raw_conj]. Beware! [(++)] has a higher
   precedence than [(&)]. *)

val add_to_conj : raw_conj -> conj -> disj

type term = Var.t * Path.t

val ex : term -> raw_conj
(** [ex x\[p\]] means "the path [p] exists in [x]." *)

val eq : term -> term -> raw_conj
(** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x] and
   [y] resp. and what's there on both sides is equal." *)

val neq : term -> term -> raw_conj
(** [ex_neq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x]
   and [y] resp. and what's there on both sides is different." *)

val abs : term -> Feat.t -> raw_conj
(** [abs x\[p\] f] means "the path [p] exists in [x] and what's there
   does not have the feature [f]." *)

val reg : term -> raw_conj
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there is
   a regular file." *)

val nreg : term -> raw_conj
(** [nreg x\[p\]] means "the path [p] exists in [x] and what's there
   is not a regular file." *)

val nex_nreg : term -> raw_conj
(** [nex_nreg x\[p\]] means "the path [p] does not exist in [x] or
   exists but is not a regular file." *)

val dir : term -> raw_conj
(** [dir x\[p\]] means "the path [p] exists in [x] and what's there is
   a directory." *)

val ndir : term -> raw_conj
(** [ndir x\[p\]] means "the path [p] exists in [x] and what's there
   isn't a directory." *)

val nex_ndir : term -> raw_conj
(** [nex_ndir x\[p\]] means "the path [p] does not exists in [x] or
   exists but is not a directory." *)

val empty : term -> raw_conj
(** [empty x\[p\]] means "the path [p] exists in [x] and what's there
   does not have any feature." *)

val nempty : term -> raw_conj
(** [nempty x\[p\]] means "the path [p] exists in [x] and what's there
   has a feature." *)

val sim1 : Var.t -> Path.t -> Var.t -> raw_conj
(** [sim1 x p y] means, with [p = q/f] "the path [q] exists in both
   [x] and [y] and [x] and [y] may only differ in the name [p]." *)

val sim2 : Var.t -> Path.t -> Path.t -> Var.t -> raw_conj
(** [sim2 x p1 p2 y] means, with [p1 = q1/f1] and [p2 = q2/f2] "the
   pathes [q1] and [q2] exist in both [x] and [y] and [x] and [y] may
   only differ in the names [p1] and [p2]." *)
