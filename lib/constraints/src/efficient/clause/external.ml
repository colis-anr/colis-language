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

let neq x y =
  with_internal_2 x y @@ fun x y ->
  neq x y

let feat x f y =
  with_internal_2 x y @@ fun x y ->
  feat x f y

let nfeat x f y =
  with_internal_2 x y @@ fun x y ->
  nfeat x f y

let abs x f =
  with_internal x @@ fun x ->
  abs x f

let nabs x f =
  with_internal x @@ fun x ->
  nabs x f

let maybe x f y =
  with_internal_2 x y @@ fun x y ->
  maybe x f y

let nmaybe x f y =
  with_internal_2 x y @@ fun x y ->
  nmaybe x f y

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

let  reg x =  kind x Constraints_common.Kind.Reg
let nreg x = nkind x Constraints_common.Kind.Reg
let  dir x =  kind x Constraints_common.Kind.Dir
let ndir x = nkind x Constraints_common.Kind.Dir
let  block x =  kind x Constraints_common.Kind.Block
let nblock x = nkind x Constraints_common.Kind.Block
let  sock x =  kind x Constraints_common.Kind.Sock
let nsock x = nkind x Constraints_common.Kind.Sock
let  pipe x =  kind x Constraints_common.Kind.Pipe
let npipe x = nkind x Constraints_common.Kind.Pipe
let  char x =  kind x Constraints_common.Kind.Char
let nchar x = nkind x Constraints_common.Kind.Char
let  symlink x =  kind x Constraints_common.Kind.Symlink
let nsymlink x = nkind x Constraints_common.Kind.Symlink

let resolve r cwd q z =
  with_internal_2 r z @@ fun r z ->
  resolve r [] (Constraints_common.Path.concat cwd q) z

let noresolve r cwd q =
  with_internal r @@ fun r ->
  noresolve r [] (Constraints_common.Path.concat cwd q)

let maybe_resolve r cwd q f c =
  let gz = Constraints_common.Var.fresh () in
  let (z, c) = Core.internalise gz c in
  Dnf.bind
    ((with_internal r @@ fun r ->
      maybe_resolve r [] (Constraints_common.Path.concat cwd q)
        (fun x -> Internal.eq x z & f gz)) c)
    (fun c -> Core.quantify_over gz c |> Dnf.single)

let similar r r' cwd q z z' =
  with_internal_2 r r' @@ fun r r' ->
  with_internal_2 z z' @@ fun z z' ->
  similar r r' Constraints_common.Path.(normalize ~cwd q) z z'
