let with_internal x f c =
  let (x, c) = Core.internalise x c in
  f x c

let with_internal_2 x y f =
  with_internal x @@ fun x ->
  with_internal y @@ fun y ->
  f x y

let check_if_need_to_stop () =
  Colis_internals.check_cpu_time_limit ();
  Colis_internals.check_memory_limit ()

open Internal

let eq x y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  eq x y

let neq x y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  neq x y

let feat x f y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  feat x f y

let nfeat x f y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  nfeat x f y

let abs x f =
  check_if_need_to_stop ();
  with_internal x @@ fun x ->
  abs x f

let nabs x f =
  check_if_need_to_stop ();
  with_internal x @@ fun x ->
  nabs x f

let maybe x f y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  maybe x f y

let nmaybe x f y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  nmaybe x f y

let fen x fs =
  check_if_need_to_stop ();
  with_internal x @@ fun x ->
  fen x fs

let nfen _x _fs =
  check_if_need_to_stop ();
  Core.not_implemented "nfen"

let sim x fs y =
  check_if_need_to_stop ();
  with_internal_2 x y @@ fun x y ->
  sim x fs y

let nsim _x _fs _y =
  check_if_need_to_stop ();
  Core.not_implemented "nsim"

let kind x k =
  check_if_need_to_stop ();
  with_internal x @@ fun x ->
  kind x k

let nkind x k =
  check_if_need_to_stop ();
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

let resolve r cwd p z =
  check_if_need_to_stop ();
  with_internal_2 r z @@ fun r z ->
  resolve r [] (Constraints_common.Path.concat cwd p) z

let noresolve r cwd p =
  check_if_need_to_stop ();
  with_internal r @@ fun r ->
  noresolve r [] (Constraints_common.Path.concat cwd p)

let maybe_resolve r cwd p z =
  check_if_need_to_stop ();
  with_internal_2 r z @@ fun r z ->
  maybe_resolve r [] (Constraints_common.Path.concat cwd p) z

let similar r r' cwd q z z' =
  check_if_need_to_stop ();
  with_internal_2 r r' @@ fun r r' ->
  with_internal_2 z z' @@ fun z z' ->
  similar r r' Constraints_common.Path.(normalize ~cwd q) z z'
