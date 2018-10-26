type conj

type disj

val ctrue : conj
val dtrue : disj
val dfalse : disj

val exists : (Var.t -> conj -> disj) -> conj -> disj

val fold : ('a -> conj -> 'a) -> 'a -> disj -> 'a
val map_to_list : (conj -> 'a) -> disj -> 'a list
(* FIXME: this is exactly (when you reverse the arguments) the monadic
   bind. But I don't dare writing it that way. *)

(** {2 Monadic functions} *)

val pure : conj -> disj

val (>>=) : disj -> (conj -> disj) -> disj

val (>=>) : (conj -> disj) -> (conj -> disj) -> (conj -> disj)

(** {2 High-level functions working on terms} *)

type term = Var.t * Path.t

val eq : term -> term -> conj -> disj
(** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x] and
   [y] resp. and what's there on both sides is equal." *)

val neq : term -> term -> conj -> disj
(** [eq x\[p\] y\[q\]] means "the pathes [p] and [q] exist in [x] and
   [y] resp. and what's there on both sides is different." *)

val abs : term -> Feat.t -> conj -> disj
(** [abs x\[p\] f] means "the path [p] exists in [x] and what's there
   does not have the feature [f]." *)

val nabs : term -> Feat.t -> conj -> disj
(** [abs x\[p\] f] means "the path [p] exists in [x] and
   what's there does have the feature [f]." *)

val reg : term -> conj -> disj
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there is
   a regular file." *)

val nreg : term -> conj -> disj
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there
   isn't a regular file." *)

val dir : term -> conj -> disj
(** [dir x\[p\]] means "the path [p] exists in [x] and what's there
   is a directory." *)

val ndir : term -> conj -> disj
(** [reg x\[p\]] means "the path [p] exists in [x] and what's there
   isn't a directory." *)

(* FIXME: We need a notation for "either [t] doesn't exist or it is
   not a directory". This is the subtle difference between (¬dir) t
   and ¬(dir t). Maybe things like "ex_and_dir", "ex_and_ndir",
   "nex_or_ndir"? *)

val fen : term -> Feat.Set.t -> conj -> disj
(** [fen x\[p\] fs] means "the path [p] exists in [x] and what's there
   does not have features that are not in [fs]." *)

val nfen : term -> Feat.Set.t -> conj -> disj
(** [fen x\[p\] fs] means "the path [p] exists in [x] and what's there
   does have a feature that is not in [fs]." *)

val empty : term -> conj -> disj
(** [empty x\[p\]] means "the path [p] exists in [x] and what's there
   does not have any feature." *)

val nempty : term -> conj -> disj
(** [nempty x\[p\]] means "the path [p] exists in [x] and what's there
   has a feature." *)

val sim1 : Var.t -> Path.t -> Var.t -> conj -> disj
(** [sim1 x p y] means, with [p = q/f] "the path [q] exists in both
   [x] and [y] and [x] and [y] may only differ in the name [p]." *)
