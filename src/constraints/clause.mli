(** This module is here to represent clauses in normal-form.
   It provides an abstract type for clauses. Any value of this type
   that has been created using this interface is guaranteed to be in
   normal-form. Such clauses in normal-form are always satisfiable.
   @see <https://hal.archives-ouvertes.fr/hal-01807474>, "Deciding the
   First-Order Theory of an Algebra of Feature Trees with Updates"
   (Nicolas Jeannerod, Ralf Treinen).
   Most of the functions in this interface will manipulate
   disjunctions {!disj} (that is, lists of {!t}). All the clauses in a
   disjunction are satisfiable. However, a function may return an
   empty disjunction if what is asked is unsatisfiable. *)

type t
(** The abstract type of clauses in normal-form. *)

val empty : t
(** The empty clause (that is, "true"). *)

type disj = t list
(** The type of disjunctions of clauses. *)

val top : disj
(** The disjunction containing only the empty clause (that is,
   "true"). *)

val bottom : disj
(** The empty disjunction (that is, "false"). *)

val sat : disj -> bool
(** [sat d] checks whether [d] is a satisfiable disjunction. This is
   infact only testing whether the disjunction is non-empty. *)

val exists : Variable.t list -> t -> t
(** [exists xs c] returns the clause corresponding to [exists xs. c]
   *)

val exists_d : Variable.t list -> disj -> disj
(** Same as {!exists} but on disjunctions. *)

val exists_comp : Variable.t list -> t -> t
(** [exists xs c] returns the clause corresponding to [exists Cxs. c]
   where [Cxs] represents the complement of [xs]. *)

val exists_comp_d : Variable.t list -> disj -> disj
(** Same as {!exists_comp} but on disjunctions. *)

val literal : Literal.t -> t -> disj
(** [literal l c] returns the disjunction corresponding to [l and
   c]. *)

val literal_d : Literal.t -> disj -> disj
(** Same as {!literal} but takes a disjunction as its input. *)

(** {3 Lower-level constructors} *)

val atom : Atom.t -> t -> disj
(** [atom a c] returns the disjunction corresponding to [a and c]. *)

val atom_d : Atom.t -> disj -> disj
(** Same as {!atom} but takes a disjunction as its input. *)

val natom : Atom.t -> t -> disj
(** [natom a c] returns the disjunction corresponding to [not a and
   c]. *)

val natom_d : Atom.t -> disj -> disj
(** Same as {!natom} but takes a disjunction as its input. *)

val eq : Variable.t -> Variable.t -> t -> disj
(** [eq x y c] returns the disjunction corresponding to [x = y and
   c]. *)

val neq : Variable.t -> Variable.t -> t -> disj
(** [neq x y c] returns the disjunction corresponding to [x != y and
   c]. *)

val feat : Variable.t -> Feature.t -> Variable.t -> t -> disj
(** [feat x f y c] returns the disjunction corresponding to [x\[f\]y
   and c]. *)

val nfeat : Variable.t -> Feature.t -> Variable.t -> t -> disj
(** [nfeat x f y c] returns the disjunction corresponding to [not
   x\[f\]y and c]. *)

val abs : Variable.t -> Feature.t -> t -> disj
(** [abs x f c] returns the disjunction corresponding to [x\[f\]^ and
   c]. *)

val nabs : Variable.t -> Feature.t -> t -> disj
(** [nabs x f c] returns the disjunction corresponding to [not x\[f\]^
   and c]. *)

val fen : Variable.t -> Feature.Set.t -> t -> disj
(** [fen x fs c] returns the disjunction corresponding to [x\[fs\] and
   c]. *)

val nfen : Variable.t -> Feature.Set.t -> t -> disj
(** [nfen x fs c] returns the disjunction corresponding to [not
   x\[fs\] and c]. *)

val sim : Variable.t -> Feature.Set.t -> Variable.t -> t -> disj
(** [sim x fs y] returns the disjunction corresponding to [x ~fs y and
   c]. *)

val nsim : Variable.t -> Feature.Set.t -> Variable.t -> t -> disj
(** [sim x fs y] returns the disjunction corresponding to [not (x ~fs
   y) and c]. *)
