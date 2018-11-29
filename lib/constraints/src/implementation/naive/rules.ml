open Constraints_common open Atom open Literal open OptionMonad

let accessibility c =
  (* Create the graph and fill it. *)
  let reach = Hashtbl.create 8 in
  let edges = Hashtbl.create 8 in
  Literal.Set.iter
    (function
     | Pos (Feat (x, _, y)) ->
        Hashtbl.replace reach y ();
        Hashtbl.add edges x y
     | _ -> ())
    c;
  (* Create a queue and add the nodes that are reachable from
     nobody. *)
  let queue = Queue.create () in
  Hashtbl.iter
    (fun x _ ->
      if not (Hashtbl.mem reach x) then
        Queue.push (x, Var.Set.empty) queue)
    edges;
  let res = Hashtbl.create 8 in
  let rec aux () =
    if not (Queue.is_empty queue) then
      (
        (* Get a node [x] and a bunch of nodes [y]s
           that lead to [x]. *)
        let (x, ys) = Queue.pop queue in
        (* We check for cycles. *)
        if Var.Set.mem x ys then
          raise (Invalid_argument "accessibility");
        (* We add the [y]s to the result: they lead to [x]. *)
        let ys =
          match Hashtbl.find_opt res x with
          | None -> ys
          | Some zs -> Var.Set.union ys zs
        in
        Hashtbl.replace res x ys;
        (* For each node [z] accessible from [x], we carry on. *)
        let ys = Var.Set.add x ys in
        Hashtbl.find_all edges x
        |> List.iter (fun z -> Queue.add (z, ys) queue);
        aux ()
      )
  in
  aux ();
  Hashtbl.fold (fun x ys l -> (x, ys) :: l) res []

let replace_var_in_atom x y = function
  | Eq (a, b) -> Eq ((if Var.equal a x then y else a), (if Var.equal b x then y else b))
  | Feat (a, f, b) -> Feat ((if Var.equal a x then y else a), f, (if Var.equal b x then y else b))
  | Abs (a, f) -> Abs ((if Var.equal a x then y else a), f)
  | Kind (a, k) -> Kind ((if Var.equal a x then y else a), k)
  | Fen (a, fs) -> Fen ((if Var.equal a x then y else a), fs)
  | Sim (a, fs, b) -> Sim ((if Var.equal a x then y else a), fs, (if Var.equal b x then y else b))

let replace_var_in_literal x y = function
  | Pos a -> Pos (replace_var_in_atom x y a)
  | Neg a -> Neg (replace_var_in_atom x y a)

let replace_var_in_literal_set x y =
  Literal.Set.map (replace_var_in_literal x y)

let (&) l s = Literal.Set.add l s

let (x, y, z) = Metavar.fresh3 ()
let f = Metavar.fresh ()
let (fs, gs) = Metavar.fresh2 ()
let (k, l) = Metavar.fresh2 ()

let c_cycle (_, c) =
  try ignore (accessibility c); None
  with Invalid_argument _ -> Some []

let c_feat_abs (_, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Abs (x, f))] in
  Pattern.find pat c >>= fun _ -> Some []

let c_feat_fen (_, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Fen (x, fs))] in
  Pattern.find
    ~pred:(fun aff ->
      not (Feat.Set.mem (Assign.feat aff f) (Assign.feat_set aff fs)))
    pat c >>= fun _ -> Some []

let c_neq_refl (_, c) =
  let pat = Pattern.[Neg (Eq (x, x))] in
  Pattern.find pat c >>= fun _ -> Some []

let c_nsim_refl (_, c) =
  let pat = Pattern.[Neg (Sim (x, fs, x))] in
  Pattern.find pat c >>= fun _ -> Some []

let c_kinds (_, c) =
  let pat = Pattern.[Pos (Kind (x, k)); Pos (Kind (x, l))] in
  Pattern.find
    ~pred:(fun aff -> Assign.kind aff k <> Assign.kind aff l)
    pat c
  >>= fun _ -> Some []

let s_eq (es, c) =
  let pat = Pattern.[Pos (Eq (x, y))] in
  Pattern.find
    ~pred:(fun aff -> Var.Set.mem (Assign.var aff x) es &&
                        not (Var.equal (Assign.var aff x) (Assign.var aff y)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  Some [Var.Set.remove x es, replace_var_in_literal_set x y c]

let s_eq_refl (es, c) =
  let pat = Pattern.[Pos (Eq (x, x))] in
  Pattern.find pat c >>= fun (_, c) ->
  Some [es, c]

let s_feats (es, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> Var.Set.mem (Assign.var aff z) es &&
                        not (Var.equal (Assign.var aff y) (Assign.var aff z)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let f = Assign.feat aff f in
  Some [Var.Set.remove z es, Pos (Feat (x, f, y)) & replace_var_in_literal_set z y c]

let s_feats_glob (es, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> not (Var.Set.mem (Assign.var aff y) es && not (Var.Set.mem (Assign.var aff z) es)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let f = Assign.feat aff f in
  Some [es, Pos (Eq (y, z)) & Pos (Feat (x, f, y)) & c]

let s_sims (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let hs = Feat.Set.inter (Assign.feat_set aff fs) (Assign.feat_set aff gs) in
  Some [es, Pos (Sim (x, hs, y)) & c]

let p_feat (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> not (Feat.Set.mem (Assign.feat aff f) (Assign.feat_set aff fs)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let f = Assign.feat aff f in
  let fs = Assign.feat_set aff fs in
  Some [es, Pos (Sim (x, fs, y)) & Pos (Feat (x, f, z)) & Pos (Feat (y, f, z)) & c]

let p_abs (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Abs (x, f))] in
  Pattern.find
    ~pred:(fun aff -> not (Feat.Set.mem (Assign.feat aff f) (Assign.feat_set aff fs)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let f = Assign.feat aff f in
  let fs = Assign.feat_set aff fs in
  Some [es, Pos (Sim (x, fs, y)) & Pos (Abs (x, f)) & Pos (Abs (y, f)) & c]

let p_fen (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Fen (x, gs))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some [es, Pos (Sim (x, fs, y)) & Pos (Fen (x, gs)) & Pos (Fen (y, Feat.Set.union fs gs)) & c]

let p_sim (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, z))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  let fgs = Feat.Set.union fs gs in
  (* This rule has a really heavy side-condition *)
  let hs =
    Literal.Set.fold
      (fun l hs ->
        match l with
        | Pos (Sim (_, hs', _)) ->
           Some (
               match hs with
               | None -> hs'
               | Some hs -> Feat.Set.inter hs hs'
             )
        | _ -> hs)
      c
      None
  in
  if match hs with
     | None -> true
     | Some hs -> not (Feat.Set.subset hs fgs)
  then
    (
      Some [es, Pos (Sim (x, fs, y)) & Pos (Sim (x, gs, z)) & Pos (Sim (y, fgs, z)) & c]
    )
  else
    None

let r_neq (es, c) =
  let pat = Pattern.[Neg (Eq (x, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  Some [es, Neg (Sim (x, Feat.Set.empty, y)) & c]

let r_nfeat (es, c) =
  let pat = Pattern.[Neg (Feat (x, f, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let f = Assign.feat aff f in
  Some [
      (es, Pos (Abs (x, f)) & c);
      let z = Var.fresh () in
      (Var.Set.add z es, Pos (Feat (x, f, z)) & Neg (Sim (y, Feat.Set.empty, z)) & c)
    ]

let r_nkind (es, c) =
  let pat = Pattern.[Neg (Kind (x, k))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let k = Assign.kind aff k in
  Some (
      Kind.all
      |> List.filter ((<>) k)
      |> List.map
           (fun k ->
             (es, Pos (Kind (x, k)) & c))
    )

let r_nabs (es, c) =
  let pat = Pattern.[Neg (Abs (x, f))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let f = Assign.feat aff f in
  let z = Var.fresh () in
  Some [Var.Set.add z es, Pos (Feat (x, f, z)) & c]

let one_feature_in x fs (es, c) =
  Feat.Set.elements fs
  |> List.map
       (fun f ->
         let z = Var.fresh () in
         (Var.Set.add z es, Pos (Feat (x, f, z)) & c))

let one_difference_in x y fs (es, c) =
  let difference_in x y f (es, c) =
    let z = Var.fresh () in
    let z' = Var.fresh () in
    [(Var.Set.add z' es, Pos (Abs (x, f)) & Pos (Feat (y, f, z')) & c) ;
     (Var.Set.add z es, Pos (Feat (x, f, z)) & Pos (Abs (y, f)) & c) ;
     (Var.Set.add z (Var.Set.add z' es), Pos (Feat (x, f, z)) & Pos (Feat (y, f, z')) & Neg (Sim (z, Feat.Set.empty, z')) & c)]
  in
  Feat.Set.elements fs
  |> List.fold_left
       (fun disj f ->
         List.map (difference_in x y f) disj
         |> List.flatten)
       [es, c]

let r_nfen_fen (es, c) =
  let pat = Pattern.[Pos (Fen (x, fs)); Neg (Fen (x, gs))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      one_feature_in
        x
        (Feat.Set.diff fs gs)
        (es, Pos (Fen (x, fs)) & c)
    )

let r_nsim_sim (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      one_difference_in
        x y
        (Feat.Set.diff fs gs)
        (es, Pos (Sim (x, fs, y)) & c)
    )

let r_nsim_fen (es, c) =
  let pat = Pattern.[Pos (Fen (x, fs)); Neg (Sim (x, gs, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      let c = Pos (Fen (x, fs)) & c in
      (es, Neg (Fen (y, Feat.Set.union fs gs)) & c)
      :: one_difference_in x y (Feat.Set.diff fs gs) (es, c)
    )

let e_nfen (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))] in
  Pattern.find
    ~pred:(fun aff ->
      not (Feat.Set.subset (Assign.feat_set aff fs) (Assign.feat_set aff gs)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      let c = Pos (Sim (x, fs, y)) & c in
      (es, Neg (Fen (x, Feat.Set.union fs gs)) & c)
      :: one_feature_in x (Feat.Set.diff fs gs) (es, c)
    )

let e_nsim (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))] in
  Pattern.find
    ~pred:(fun aff ->
      not (Feat.Set.subset (Assign.feat_set aff fs) (Assign.feat_set aff gs)))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      let c = Pos (Sim (x, fs, y)) & c in
      (es, Neg (Sim (x, Feat.Set.union fs gs, z)) & c)
      :: one_difference_in x z (Feat.Set.diff fs gs) (es, c)
    )

let p_nfen (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))] in
  Pattern.find
    ~pred:(fun aff ->
      Feat.Set.subset (Assign.feat_set aff fs) (Assign.feat_set aff gs))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some [es, Pos (Sim (x, fs, y)) & Neg (Fen (x, gs)) & Neg (Fen (y, gs)) & c]

let p_nsim (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))] in
  Pattern.find
    ~pred:(fun aff ->
      Feat.Set.subset (Assign.feat_set aff fs) (Assign.feat_set aff gs))
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let y = Assign.var aff y in
  let z = Assign.var aff z in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some [es, Pos (Sim (x, fs, y)) & Neg (Sim (x, gs, z)) & Neg (Sim (y, gs, z)) & c]

let s_kind (es, c) =
  let pat = Pattern.[Pos (Kind (x, k))] in
  Pattern.find
    ~pred:(fun aff -> Assign.kind aff k <> Dir)
    pat c
  >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let k = Assign.kind aff k in
  Some [es, Pos (Fen (x, Feat.Set.empty)) & Pos (Kind (x, k)) & c]

(* ========================================================================== *)
(* ============================= [ All Rules ] ============================== *)
(* ========================================================================== *)

let all = [
    "C-Cycle",      c_cycle;
    "C-Feat-Abs",   c_feat_abs;
    "C-Feat-Fen",   c_feat_fen;
    "C-NEq-Refl",   c_neq_refl;
    "C-NSim-Refl",  c_nsim_refl;
    "C-Kinds",      c_kinds;
    "S-Eq",         s_eq;
    "S-Eq-Refl",    s_eq_refl;
    "S-Feats",      s_feats;
    "S-Feats-Glob", s_feats_glob;
    "S-Sims",       s_sims;
    "S-Kind",       s_kind;
    "P-Feats",      p_feat;
    "P-Abs",        p_abs;
    "P-Fen",        p_fen;
    "P-Sim",        p_sim;
    "R-Neq",        r_neq;
    "R-NFeat",      r_nfeat;
    "R-NAbs",       r_nabs;
    "R-NFen-Fen",   r_nfen_fen;
    "R-NSim-Sim",   r_nsim_sim;
    "R-NSim-Fen",   r_nsim_fen;
    "R-NKind",      r_nkind;
    "E-NFen",       e_nfen;
    "E-NSim",       e_nsim;
    "P-NFen",       p_nfen;
    "P-NSim",       p_nsim;
  ]
