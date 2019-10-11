open Colis_constraints_common open Atom open Literal

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

let replace_in_atom ~var:x ~by:y = function
  | Eq (a, b) -> Eq ((if Var.equal a x then y else a), (if Var.equal b x then y else b))
  | Feat (a, f, b) -> Feat ((if Var.equal a x then y else a), f, (if Var.equal b x then y else b))
  | Abs (a, f) -> Abs ((if Var.equal a x then y else a), f)
  | Kind (a, k) -> Kind ((if Var.equal a x then y else a), k)
  | Fen (a, fs) -> Fen ((if Var.equal a x then y else a), fs)
  | Sim (a, fs, b) -> Sim ((if Var.equal a x then y else a), fs, (if Var.equal b x then y else b))

let replace_in_literal ~var ~by = function
  | Pos a -> Pos (replace_in_atom ~var ~by a)
  | Neg a -> Neg (replace_in_atom ~var ~by a)

let replace_in_literal_set ~var ~by =
  Literal.Set.map (replace_in_literal ~var ~by)

let (&) l s = Literal.Set.add l s

let (x, y, z) = Metavar.fresh3 ()
let f = Metavar.fresh ()
let (fs, gs) = Metavar.fresh2 ()
let (k, l) = Metavar.fresh2 ()

let c_cycle (_, c) =
  try ignore (accessibility c); None
  with Invalid_argument _ -> Some []

let make ~pat ?pred ~prod () =
  fun (es, conj) ->
  Pattern.find_all ?pred pat (es, conj)
  |> Seq.filter_map
       (fun (a, conj') ->
         (* The production rule gives us a disjunction. However,
            sometimes, this conjunction might be equal to the given
            one. In this case, we have to keep looking for an other
            production rule that might be better. *)
         match prod a conj' with
         | [] -> Some []
         | [conj'] when Conj.equal conj' (es, conj) -> None
         | [conj'] -> Some [conj']
         | disj' ->
            assert (List.for_all (fun conj' -> not (Conj.equal conj' (es, conj))) disj');
            Some disj')
  |> (fun seq -> seq ())
  |> function
    | Nil -> None
    | Cons (x, _) -> Some x

let clash _ _ = []

let c_feat_abs =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Abs (x, f))]
    ~prod:clash
    ()

let c_feat_fen =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Fen (x, fs))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:clash
    ()

let c_neq_refl =
  make
    ~pat:[Neg (Eq (x, x))]
    ~prod:clash
    ()

let c_nsim_refl =
  make
    ~pat:[Neg (Sim (x, fs, x))]
    ~prod:clash
    ()

let c_kinds =
  make
    ~pat:[Pos (Kind (x, k)); Pos (Kind (x, l))]
    ~pred:(fun a _ -> Assign.kind a k <> Assign.kind a l)
    ~prod:clash
    ()

let s_eq_glob =
  (* Note: this rule is not in the article but is necessary here. This is
     because we need to check for satifiability of formulas, which is not what
     is stated in the article. *)
  make
    ~pat:[Pos (Eq (x, y))]
    ~pred:(fun a _ ->
        not (Var.equal (Assign.var a x) (Assign.var a y)))
    ~prod:(fun a (es, c) ->
        let x = Assign.var a x in
        let y = Assign.var a y in
        let (x, y) =
          (* Replace the newest one by the oldest one. *)
          match Var.compare x y with
          | c when c < 0 -> (y, x)
          | c when c > 0 -> (x, y)
          | _ -> assert false
        in
        [es, Pos (Eq (x, y)) & replace_in_literal_set ~var:x ~by:y c])
    ()

let s_eq =
  make
    ~pat:[Pos (Eq (x, y))]
    ~pred:(fun a (es, _) ->
      Var.Set.mem (Assign.var a x) es
      && not (Var.equal (Assign.var a x) (Assign.var a y)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      [Var.Set.remove x es, replace_in_literal_set ~var:x ~by:y c])
    ()

let s_eq_refl =
  make
    ~pat:[Pos (Eq (x, x))]
    ~prod:(fun _ (es, c) -> [es, c])
    ()

let s_feats =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a (es, _) ->
      Var.Set.mem (Assign.var a z) es
      && not (Var.equal (Assign.var a y) (Assign.var a z)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let f = Assign.feat a f in
      [Var.Set.remove z es, Pos (Feat (x, f, y)) & replace_in_literal_set ~var:z ~by:y c])
    ()

let s_feats_glob =
  make
    ~pat:[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a (es, _) ->
      not (Var.Set.mem (Assign.var a y) es)
      && not (Var.Set.mem (Assign.var a z) es))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let f = Assign.feat a f in
      [es, Pos (Eq (y, z)) & Pos (Feat (x, f, y)) & c])
    ()

let s_sims =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, y))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let hs = Feat.Set.inter (Assign.feat_set a fs) (Assign.feat_set a gs) in
      [es, Pos (Sim (x, hs, y)) & c])
    ()

let p_feat =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let f = Assign.feat a f in
      let fs = Assign.feat_set a fs in
      [es, Pos (Sim (x, fs, y)) & Pos (Feat (x, f, z)) & Pos (Feat (y, f, z)) & c])
    ()

let p_abs =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Abs (x, f))]
    ~pred:(fun a _ -> not (Feat.Set.mem (Assign.feat a f) (Assign.feat_set a fs)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let f = Assign.feat a f in
      let fs = Assign.feat_set a fs in
      [es, Pos (Sim (x, fs, y)) & Pos (Abs (x, f)) & Pos (Abs (y, f)) & c])
    ()

let p_fen =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Fen (x, gs))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      [es, Pos (Sim (x, fs, y)) & Pos (Fen (x, gs)) & Pos (Fen (y, Feat.Set.union fs gs)) & c])
    ()

let p_sim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Pos (Sim (x, gs, z))]
    ~pred:(fun a (_, c) ->
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
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
      match hs with
      | None -> true
      | Some hs -> not (Feat.Set.subset hs fgs))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      let fgs = Feat.Set.union fs gs in
      [es, Pos (Sim (x, fs, y)) & Pos (Sim (x, gs, z)) & Pos (Sim (y, fgs, z)) & c])
    ()

let r_neq =
  make
    ~pat:[Neg (Eq (x, y))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      [es, Neg (Sim (x, Feat.Set.empty, y)) & c])
    ()

let r_nfeat =
  make
    ~pat:[Neg (Feat (x, f, y))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let f = Assign.feat a f in
      [
        (es, Pos (Abs (x, f)) & c);
        let z = Var.fresh () in
        (Var.Set.add z es, Pos (Feat (x, f, z)) & Neg (Sim (y, Feat.Set.empty, z)) & c)
      ]
    )
    ()

let r_nkind =
  make
    ~pat:[Neg (Kind (x, k))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let k = Assign.kind a k in
      Kind.all
      |> List.filter ((<>) k)
      |> List.map
           (fun k ->
             (es, Pos (Kind (x, k)) & c))
    )
    ()

let r_nabs =
  make
    ~pat:[Neg (Abs (x, f))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let f = Assign.feat a f in
      let z = Var.fresh () in
      [Var.Set.add z es, Pos (Feat (x, f, z)) & c])
    ()

let one_feature_in x fs es c =
  Feat.Set.elements fs
  |> List.map
       (fun f ->
         let z = Var.fresh () in
         (Var.Set.add z es, Pos (Feat (x, f, z)) & c))

let one_difference_in x y fs es c =
  let difference_in x y f es c =
    let z = Var.fresh () in
    let z' = Var.fresh () in
    [(Var.Set.add z' es, Pos (Abs (x, f)) & Pos (Feat (y, f, z')) & c) ;
     (Var.Set.add z es, Pos (Feat (x, f, z)) & Pos (Abs (y, f)) & c) ;
     (Var.Set.add z (Var.Set.add z' es), Pos (Feat (x, f, z)) & Pos (Feat (y, f, z')) & Neg (Sim (z, Feat.Set.empty, z')) & c)]
  in
  Feat.Set.elements fs
  |> List.map
       (fun f ->
         difference_in x y f es c)
  |> List.flatten

let r_nfen_fen =
  make
    ~pat:[Pos (Fen (x, fs)); Neg (Fen (x, gs))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      one_feature_in
        x (Feat.Set.diff fs gs)
        es (Pos (Fen (x, fs)) & c)
    )
    ()

let r_nsim_sim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, y))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      one_difference_in
        x y (Feat.Set.diff fs gs)
        es (Pos (Sim (x, fs, y)) & c)
    )
    ()

let r_nsim_fen =
  make
    ~pat:[Pos (Fen (x, fs)); Neg (Sim (x, gs, y))]
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      let c = Pos (Fen (x, fs)) & c in
      (es, Neg (Fen (y, Feat.Set.union fs gs)) & c)
      :: one_difference_in x y (Feat.Set.diff fs gs) es c
    )
    ()

let e_nfen =
  make
    ~pat:Pattern.[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))]
    ~pred:(fun a _ -> not (Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      let c = Pos (Sim (x, fs, y)) & c in
      (es, Neg (Fen (x, Feat.Set.union fs gs)) & c)
      :: one_feature_in x (Feat.Set.diff fs gs) es c
    )
    ()

let e_nsim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))]
    ~pred:(fun a _ -> not (Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs)))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      let c = Pos (Sim (x, fs, y)) & c in
      (es, Neg (Sim (x, Feat.Set.union fs gs, z)) & c)
      :: one_difference_in x z (Feat.Set.diff fs gs) es c
    )
    ()

let p_nfen =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Fen (x, gs))]
    ~pred:(fun a _ -> Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      [es, Pos (Sim (x, fs, y)) & Neg (Fen (x, gs)) & Neg (Fen (y, gs)) & c])
    ()

let p_nsim =
  make
    ~pat:[Pos (Sim (x, fs, y)); Neg (Sim (x, gs, z))]
    ~pred:(fun a _ -> Feat.Set.subset (Assign.feat_set a fs) (Assign.feat_set a gs))
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let y = Assign.var a y in
      let z = Assign.var a z in
      let fs = Assign.feat_set a fs in
      let gs = Assign.feat_set a gs in
      [es, Pos (Sim (x, fs, y)) & Neg (Sim (x, gs, z)) & Neg (Sim (y, gs, z)) & c])
    ()

let s_kind =
  make
    ~pat:[Pos (Kind (x, k))]
    ~pred:(fun a _ -> Assign.kind a k <> Dir)
    ~prod:(fun a (es, c) ->
      let x = Assign.var a x in
      let k = Assign.kind a k in
      [es, Pos (Fen (x, Feat.Set.empty)) & Pos (Kind (x, k)) & c])
    ()

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
    "S-Eq-Glob",    s_eq_glob;
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
