type t = Core.t

let true_ = Core.empty

let make_initial = Core.make_initial

let quantify_over _x c = Dnf.single c (* FIXME !!! *)

let exists f c =
  let x = Core.fresh_var () in
  Dnf.bind (f x c) (quantify_over x)

(* let quantify_over x c =
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
  [c] *)

let with_internal x f c =
  let (x, c) = Core.internalise x c in
  f x c

let with_internal_2 x y f =
  with_internal x @@ fun x ->
  with_internal y @@ fun y ->
  f x y

let with_internal_l xs f c =
  let (xs, c) =
    List.fold_left
      (fun (xs, c) x ->
         let (x, c) = Core.internalise x c in
         (x :: xs, c))
      ([], c) (List.rev xs)
  in
  f xs c

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

let maybe x f ys =
  with_internal_l ys @@ fun ys ->
  maybe x f ys

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
