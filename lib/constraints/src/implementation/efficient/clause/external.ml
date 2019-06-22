open Constraints_common

type t = Core.t

let true_ = Core.empty

let quantify_over x c =
  let c = { c with Core.globals = IVar.quantify_over x c.Core.globals } in
  (* Garbage collection. We first find all the variables that are accessible
     from global variables. This could be made much more efficient by keeping
     for each variable from which global variable it is accessible. *)
  let accessibles = ref IVar.Set.empty in
  let add_accessible x =
    accessibles := IVar.Set.add (IVar.repr c.info x) !accessibles
  in
  let rec set_accessible_and_crawl_down x =
    if not (IVar.Set.mem x !accessibles) then
      add_accessible x;
      let info_x = IVar.get c.info x in
      match info_x.kind with
      | Dir d ->
        Feat.Map.iter
          (fun _ -> function
             | Core.Pointsto y -> set_accessible_and_crawl_down y
             | _ -> ())
          d.feats
      | _ -> ()
  in
  IVar.iter_globals c.globals (fun _ x -> set_accessible_and_crawl_down x);
  let accessible x = IVar.Set.mem x !accessibles in
  (* We then remove all the variables that are *not* accessible from global
     variables. See lemma in article. *)
  let globals =
    IVar.map_globals
      c.globals
      (fun x -> IVar.repr c.info x)
  in
  let info = IVar.filter c.info accessible in
  let info =
    IVar.map
      info
      (fun i ->
         Core.{
           initial = i.initial ;
           nfeats =
             i.nfeats
             |> List.map (fun (f, y) -> (f, IVar.repr c.info y))
             |> List.filter (fun (_, y) -> accessible y)
           ;
           nabs = i.nabs ;
           nfens = i.nfens ;
           nsims =
             i.nsims
             |> List.map (fun (fs, y) -> (fs, IVar.repr c.info y))
             |> List.filter (fun (_, y) -> accessible y)
           ;
           kind =
             match i.kind with
             | Dir d ->
               Dir
                 { fen = d.fen ;
                   sims =
                     d.sims
                     |> List.map (fun (fs, y) -> (fs, IVar.repr c.info y))
                     |> List.filter (fun (_, y) -> accessible y)
                 ;
                   feats =
                     d.feats
                     |> Feat.Map.map
                       (function
                         | Pointsto y -> Pointsto (IVar.repr c.info y)
                         | target -> target)
                     |> Feat.Map.filter
                       (fun _f -> function
                          | Pointsto y -> accessible y
                          | _ -> true)
                 }
             | kind -> kind
         }
      )
  in
  let c = Core.{ globals ; info } in
  (* Technically, we do not need to return a list. But that makes the interface
     more consistent. *)
  [c]

let internalise (x : Var.t) c =
  let (x, globals, info) = IVar.internalise x c.Core.globals c.info Core.empty_info in
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

let neq _x _y = not_implemented "neq"

let feat x f y =
  with_internal_2 x y @@ fun x y ->
  feat x f y

let nfeat _x _f _y = not_implemented "nfeat"

let abs x f =
  with_internal x @@ fun x ->
  abs x f

let nabs x f =
  with_internal x @@ fun x ->
  nabs x f

let fen x fs =
  with_internal x @@ fun x ->
  fen x fs

let nfen _x _fs = not_implemented "nfen"

let sim x fs y =
  with_internal_2 x y @@ fun x y ->
  sim x fs y

let nsim _x _fs _y = not_implemented "nsim"

let kind k x =
  with_internal x @@ fun x ->
  kind k x

let nkind k x =
  with_internal x @@ fun x ->
  nkind k x
