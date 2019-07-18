open Constraints_common
open Constraints_efficient_clause
include External

type sat_conj = Core.t
let true_sat_conj = Core.empty

let pp_sat_conj = PrettyPrinter.pp
let pp_sat_conj_as_dot = PrettyPrinter.pp_as_dot

type t = sat_conj -> sat_conj Dnf.t
let true_ = Dnf.single

(* FIXME: *)
let kind k x = kind x k
let nkind k x = nkind x k

let  reg =  kind Kind.Reg
let nreg = nkind Kind.Reg
let  dir =  kind Kind.Dir
let ndir = nkind Kind.Dir
let  block =  kind Kind.Block
let nblock = nkind Kind.Block
let  sock =  kind Kind.Sock
let nsock = nkind Kind.Sock
let  pipe =  kind Kind.Pipe
let npipe = nkind Kind.Pipe
let  char =  kind Kind.Char
let nchar = nkind Kind.Char
let  symlink =  kind Kind.Symlink
let nsymlink = nkind Kind.Symlink

let  sim1 x f y = sim x (Feat.Set.singleton f) y

let quantify_over x c =
  Core.quantify_over x c |> Dnf.single

let make_initial = Core.make_initial

let exists ?hint f = fun c ->
  let x = Var.fresh ?hint () in
  c |> f x |> List.map (Core.quantify_over x)

let exists2 ?hint1 ?hint2 f =
  exists ?hint:hint1 @@ fun x ->
  exists ?hint:hint2 @@ fun y ->
  f x y

let exists3 ?hint1 ?hint2 ?hint3 f =
  exists ?hint:hint1 @@ fun x ->
  exists ?hint:hint2 @@ fun y ->
  exists ?hint:hint3 @@ fun z ->
  f x y z

let and_ r1 r2 = fun c ->
  c |> r1 |> List.map r2 |> List.flatten

let (&) = and_

let add_to_sat_conj = (@@)

let or_ r1 r2 = fun c ->
  (c |> r1) @ (c |> r2)

let rec resolve_stack x pi q z =
  match Path.split_first_rel q with
  | None -> eq x z
  | Some (Down f, q) ->
    exists ~hint:(Feat.to_string f) @@ fun y ->
    feat x f y & resolve_stack y (x :: pi) q z
  | Some (Here, q) ->
    resolve_stack x pi q z
  | Some (Up, q) ->
    match pi with
    | [] -> resolve_stack x [] q z
    | y::pi -> dir x & resolve_stack y pi q z

let resolve r cwd q z =
  resolve_stack r [] (Path.concat cwd q) z

let rec noresolve_stack x pi q =
  match Path.split_first_rel q with
  | None -> (fun _ -> []) (* false *)
  | Some (Down f, q) ->
    (
      match Path.split_first_rel q with
      | None ->
        abs x f
      | _ ->
        exists ~hint:(Feat.to_string f) @@ fun y ->
        maybe x f y & noresolve_stack y (x::pi) q
    )
  | Some (Here, q) ->
    noresolve_stack x pi q
  | Some (Up, q) ->
    match pi with
    | [] -> noresolve_stack x [] q
    | y::pi -> or_ (ndir x) (noresolve_stack y pi q)

let noresolve r cwd q =
  dir r & noresolve_stack r [] (Path.concat cwd q)

let rec similar_normal x x' p z z' =
  match p with
  | [] ->
    eq x z & eq x' z'
  | f::p ->
    exists2 @@ fun y y' ->
    feat x f y & feat x' f y' & sim1 x f x' & similar_normal y y' p z z'

let similar r r' cwd q z z' =
  similar_normal r r' Path.(normalize ~cwd q) z z'
