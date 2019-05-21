open Constraints_common

type t = Core.t

let true_ = Core.empty

let quantify_over x c =
  (* FIXME: garbage collection *) (* Technically, we do not need to return a
  list. But that makes the interface more consistent. *)
  [{ c with Core.globals = IVar.quantify_over x c.Core.globals }]

let internalise (x : Var.t) c =
  let (x, globals) = IVar.internalise x c.Core.globals in
  (x, { c with globals })

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

let (@@@) f g x = f x @@ g

let  reg = with_internal @@@  reg
let nreg = with_internal @@@ nreg
let  dir = with_internal @@@  dir
let ndir = with_internal @@@ ndir
let  block = with_internal @@@  block
let nblock = with_internal @@@ nblock
let  char = with_internal @@@  char
let nchar = with_internal @@@ nchar
let  pipe = with_internal @@@  pipe
let npipe = with_internal @@@ npipe
let  symlink = with_internal @@@  symlink
let nsymlink = with_internal @@@ nsymlink
let  sock = with_internal @@@  sock
let nsock = with_internal @@@ nsock
