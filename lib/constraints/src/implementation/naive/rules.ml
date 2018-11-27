open Constraints_common open Atom open Literal open OptionMonad

let replace_var_in_atom x y = function
  | Eq (a, b) -> Eq ((if Var.equal a x then y else a), (if Var.equal b x then y else b))
  | Feat (a, f, b) -> Feat ((if Var.equal a x then y else a), f, (if Var.equal b x then y else b))
  | Abs (a, f) -> Abs ((if Var.equal a x then y else a), f)
  | Reg a -> Reg (if Var.equal a x then y else a)
  | Dir a -> Dir (if Var.equal a x then y else a)
  | Fen (a, fs) -> Fen ((if Var.equal a x then y else a), fs)
  | Sim (a, fs, b) -> Sim ((if Var.equal a x then y else a), fs, (if Var.equal b x then y else b))

let replace_var_in_literal x y = function
  | Pos a -> Pos (replace_var_in_atom x y a)
  | Neg a -> Neg (replace_var_in_atom x y a)

let replace_var_in_literal_set x y =
  Literal.Set.map (replace_var_in_literal x y)

let (&) l s = Literal.Set.add l s

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()

(* FIXME: C-Cycle *)

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

let r_nfen_fen (es, c) =
  let pat = Pattern.[Pos (Fen (x, fs)); Neg (Fen (x, gs))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Assign.var aff x in
  let fs = Assign.feat_set aff fs in
  let gs = Assign.feat_set aff gs in
  Some (
      let c = Literal.Set.add (Pos (Fen (x, fs))) c in
      Feat.Set.diff fs gs
      |> Feat.Set.elements
      |> List.map
           (fun f ->
             let z = Var.fresh () in
             (Var.Set.add z es, Pos (Feat (x, f, z)) & c))
    )

let all : (string * (Conj.t -> Conj.disj option)) list = [
    "C-Cycle",      c_cycle;
    "C-Feat-Abs",   c_feat_abs;
    "C-Feat-Fen",   c_feat_fen;
    "C-NEq-Refl",   c_neq_refl;
    "C-NSim-Refl",  c_nsim_refl;
    "S-Eq",         s_eq;
    "S-Feats",      s_feats;
    "S-Feats-Glob", s_feats_glob;
    "S-Sims",       s_sims;
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
    "E-NFen",       e_nfen;
    "E-NSim",       e_nsim;
  ]
