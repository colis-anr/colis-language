(** {1 Unsafe Constraints Modifiers}

    The functions in this module are unsafe in the sense that:

    - they do not care about propagating information,
    - they often have preconditions like "being a directory". *)

open Constraints_common

val feat : IVar.t -> Feat.t -> IVar.t -> Core.t -> Core.t list
(** [feat x f y] adds the feature [x[f\]y] to [x]'s info.

    It assumes that [x] has kind [Dir d] and that, in [d.feats] at indice [f],
    there is nothing (when [d.fen = false]) or [DontKnow] or [Exists].

    It does not handle propagation. *)

val abs : IVar.t -> Feat.t -> Core.t -> Core.t list
(** [abs x f] adds the absence [x[f\]↑]] to [x]'s [info].

    It assumes that [x] has kind [Dir d].

    It looks in [d.feats] for [f].

    If there is nothing and if, additionnally, there is a fence in [d], it does
    nothing.

    Otherwise, if there is nothing, [DontKnow] or [Noresolve _], it adds
    [Noresolve [\]].

    If there is [Exists] or [Pointsto _], it returns the empty Dnf.

    It does not handle propagation. *)

val nabs : IVar.t -> Feat.t -> Core.t -> Core.t list

(** [nabs x f] adds the negated absence [¬x[f\]↑] to [x]'s [info].

    It does not assume [x]'s kind. However, its behaviour depends on it.

    If [x]'s kind is not [Dir _], it adds [f] to [info.nabs] if it is not here
    already.

    If [x]'s kind is [Dir d], it looks at [d.feats] at indice [f].

    If there is nothing and if, additionnally, there is a fence in [d], it
    returns the empty Dnf.

    Otherwise, if there is nothing or [DontKnow], [Exists] is added.

    If there is [Exists] or [Pointsto _], nothing is done.

    If there is [Noresolve _], it fails with [assert false]. This will change in
    the future. FIXME.

    It does not handle propagation. *)

val fen : IVar.t -> Feat.Set.t -> Core.t -> Core.t list

(** [fen x fs] adds the fence [x[fs\]] to [x]'s info.

    It assumes that [x] has kind [Dir dir].

    It ensures that all indices outside [fs] in [dir.feats] are [DontKnow] or
    [Noresolve] (and removes them).

    It ensures that all indices inside [fs] in [dir.feats] are represented
    (adding them as [DontKnow] if required).

    It does not handle propagation. *)

val sim : IVar.t -> Feat.Set.t -> IVar.t -> Core.t -> Core.t list

(** [sim x fs y] adds the similarity [x ~fs y] to [x]'s info and [y ~fs x] to
    [y]'s info.

    It assumes that [x] and [y] have kind [Dir _].

    If there is already a similarity, the index is the intersection of the
    current index and [fs].

    It does not handle propagation. *)
