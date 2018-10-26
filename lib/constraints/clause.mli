(** {1 Clauses} *)

type lit
(** Abstract type for literals. *)

type conj
(** Abstract type for satisfiable conjunctions of {!lit}. *)

type disj
(** Abstract type for disjunctions of {!conj}. *)

(** {2 Literals} *)

val ltrue : lit

val exists : (Var.t -> lit) -> lit

val (&) : lit -> lit -> lit

val add_to_conj : lit -> conj -> disj

type term = Var.t * Path.t

val eq : term -> term -> lit
(** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x] and
   [y] resp. and what's there on both sides is equal." *)

val neq : term -> term -> lit
(** [ex_neq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x]
   and [y] resp. and what's there on both sides is different." *)

val abs : term -> Feat.t -> lit
(** [abs x\[p\] f] means "the path [p] exists in [x] and what's there
   does not have the feature [f]." *)

val nabs : term -> Feat.t -> lit
(** [nabs x\[p\] f] means "the path [p] exists in [x] and what's there
   does have the feature [f]." *)

val reg : term -> lit
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there is
   a regular file." *)

val nreg : term -> lit
(** [nreg x\[p\]] means "the path [p] exists in [x] and what's there
   is not a regular file." *)

val nex_nreg : term -> lit
(** [nex_nreg x\[p\]] means "the path [p] does not exist in [x] or
   exists but is not a regular file." *)

val dir : term -> lit
(** [dir x\[p\]] means "the path [p] exists in [x] and what's there is
   a directory." *)

val ndir : term -> lit
(** [ndir x\[p\]] means "the path [p] exists in [x] and what's there
   isn't a directory." *)

val nex_ndir : term -> lit
(** [nex_ndir x\[p\]] means "the path [p] does not exists in [x] or
   exists but is not a directory." *)

val empty : term -> lit
(** [empty x\[p\]] means "the path [p] exists in [x] and what's there
   does not have any feature." *)

val nempty : term -> lit
(** [nempty x\[p\]] means "the path [p] exists in [x] and what's there
   has a feature." *)

val sim1 : Var.t -> Path.t -> Var.t -> lit
(** [sim1 x p y] means, with [p = q/f] "the path [q] exists in both
   [x] and [y] and [x] and [y] may only differ in the name [p]." *)

val sim2 : Var.t -> Path.t -> Path.t -> Var.t -> lit
(** [sim2 x p1 p2 y] means, with [p1 = q1/f1] and [p2 = q2/f2] "the
   pathes [q1] and [q2] exist in both [x] and [y] and [x] and [y] may
   only differ in the names [p1] and [p2]." *)

(** {2 Other} *)

val ctrue : conj
val dtrue : disj

val disjuncts : disj -> conj list




(* val fold : ('a -> conj -> 'a) -> 'a -> disj -> 'a
 * val map_to_list : (conj -> 'a) -> disj -> 'a list
 * (\* FIXME: this is exactly (when you reverse the arguments) the monadic
 *    bind. But I don't dare writing it that way. *\)
 *
 * (\** {2 Monadic functions} *\)
 *
 * val pure : conj -> disj
 *
 * val (>>=) : disj -> (conj -> disj) -> disj
 *
 * val (>=>) : (conj -> disj) -> (conj -> disj) -> (conj -> disj)
 *
 * (\** {2 High-level functions working on terms} *\) *)
