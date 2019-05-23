open Constraints_common

type t = Core.t

let true_ = Core.empty

let quantify_over x c =
  (* FIXME: garbage collection *) (* Technically, we do not need to return a
  list. But that makes the interface more consistent. *)
  [{ c with Core.globals = IVar.quantify_over x c.Core.globals }]

let internalise (x : Var.t) c =
  let (x, globals, info ) = IVar.internalise x c.Core.globals c.info Core.empty_info in
  (x, Core.{ globals ; info })

let with_internal x f c =
  let (x, c) = internalise x c in
  f x c

let with_internal_2 x y f =
  with_internal x @@ fun x ->
  with_internal y @@ fun y ->
  f x y

open Safe

let eq x y =
  with_internal_2 x y @@ fun x y ->
  eq x y

let neq _x _y = assert false

let feat x f y =
  with_internal_2 x y @@ fun x y ->
  feat x f y

let nfeat _x _f _y = assert false

let abs x f =
  with_internal x @@ fun x ->
  abs x f

let nabs x f =
  with_internal x @@ fun x ->
  nabs x f

let fen x fs =
  with_internal x @@ fun x ->
  fen x fs

let nfen _x _fs = assert false

let sim x fs y =
  with_internal_2 x y @@ fun x y ->
  sim x fs y

let nsim _x _fs _y = assert false

let kind k x =
  with_internal x @@ fun x ->
  kind k x

let nkind k x =
  with_internal x @@ fun x ->
  nkind k x
