type t = Core.t

let true_ = Core.empty

let make_initial = Core.make_initial

let quantify_over x c =
  Dnf.single (Core.quantify_over x c) (* FIXME: garbage collection *)

let exists f c =
  (* fresh_var already returns an existentially quantified variable *)
  let (x, c) = Core.fresh_var c in
  f x c

let with_internal x f c =
  let (x, c) = Core.internalise x c in
  f x c

let with_internal_2 x y f =
  with_internal x @@ fun x ->
  with_internal y @@ fun y ->
  f x y

open Internal

let eq x y =
  with_internal_2 x y @@ fun x y ->
  eq x y

let neq _x _y =
  Core.not_implemented "neq"

let feat x f y =
  with_internal_2 x y @@ fun x y ->
  feat x f y

let nfeat _x _f _y =
  (* exists @@ fun z -> maybe x f z & neq y z *)
  Core.not_implemented "nfeat"

let abs x f =
  with_internal x @@ fun x ->
  abs x f

let nabs x f =
  (* FIXME: should probably go in Internal. *)
  with_internal x @@ fun x ->
  exists @@ fun z ->
  Internal.feat x f z

let maybe x f y =
  with_internal_2 x y @@ fun x y ->
  maybe x f y

let nmaybe _x _f _y =
  Core.not_implemented "nmaybe"

let fen x fs =
  with_internal x @@ fun x ->
  fen x fs

let nfen _x _fs =
  Core.not_implemented "nfen"

let sim x fs y =
  with_internal_2 x y @@ fun x y ->
  sim x fs y

let nsim _x _fs _y =
  Core.not_implemented "nsim"

let kind x k =
  with_internal x @@ fun x ->
  kind x k

let nkind x k =
  with_internal x @@ fun x ->
  nkind x k
