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

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()


let c_feat_abs (_, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Abs (x, f))] in
  Pattern.find pat c >>= fun _ -> Some []

let c_neq_refl (_, c) =
  let pat = Pattern.[Neg (Eq (x, x))] in
  Pattern.find pat c >>= fun _ -> Some []

let c_nsim_refl (_, c) =
  let pat = Pattern.[Neg (Sim (x, fs, x))] in
  Pattern.find pat c >>= fun _ -> Some []

let s_eq (es, c) =
  let pat = Pattern.[Pos (Eq (x, y))] in
  Pattern.find
    ~pred:(fun aff -> Var.Set.mem (Affect.var aff x) es &&
                        not (Var.equal (Affect.var aff x) (Affect.var aff y)))
    pat c
  >>= fun (aff, c) ->
  let x = Affect.var aff x in
  let y = Affect.var aff y in
  Some [
      Var.Set.remove x es,
      replace_var_in_literal_set x y c;
    ]

let s_feats (es, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> Var.Set.mem (Affect.var aff z) es &&
                        not (Var.equal (Affect.var aff y) (Affect.var aff z)))
    pat c
  >>= fun (aff, c) ->
  let x = Affect.var aff x in
  let y = Affect.var aff y in
  let z = Affect.var aff z in
  let f = Affect.feat aff f in
  Some [
      Var.Set.remove z es,
      replace_var_in_literal_set z y c
      |> Literal.Set.add (Pos (Feat (x, f, y)))
    ]

let s_feats_glob (es, c) =
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> not (Var.Set.mem (Affect.var aff y) es && not (Var.Set.mem (Affect.var aff z) es)))
    pat c
  >>= fun (aff, c) ->
  let x = Affect.var aff x in
  let y = Affect.var aff y in
  let z = Affect.var aff z in
  let f = Affect.feat aff f in
  Some [
      es,
      c (* FIXME: maybe replace right from here? *)
      |> Literal.Set.add (Pos (Feat (x, f, y)))
      |> Literal.Set.add (Pos (Eq (y, z)))
    ]

let p_feat (es, c) =
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))] in
  Pattern.find
    ~pred:(fun aff -> not (Feat.Set.mem (Affect.feat aff f) (Affect.feat_set aff fs)))
    pat c
  >>= fun (aff, c) ->
  let x = Affect.var aff x in
  let y = Affect.var aff y in
  let z = Affect.var aff z in
  let f = Affect.feat aff f in
  let fs = Affect.feat_set aff fs in
  Some [
      es,
      c
      |> Literal.Set.add (Pos (Sim (x, fs, y)))
      |> Literal.Set.add (Pos (Feat (x, f, z)))
      |> Literal.Set.add (Pos (Feat (y, f, z)))
    ]

let r_nfeat (es, c) =
  let pat = Pattern.[Neg (Feat (x, f, y))] in
  Pattern.find pat c >>= fun (aff, c) ->
  let x = Affect.var aff x in
  let y = Affect.var aff y in
  let f = Affect.feat aff f in
  Some [
      (es,
       c
       |> Literal.Set.add (Pos (Abs (x, f))));
      let z = Var.fresh () in
      (Var.Set.add z es,
       c
       |> Literal.Set.add (Pos (Feat (x, f, z)))
       |> Literal.Set.add (Neg (Sim (y, Feat.Set.empty, z))))
    ]

let r_nfen_fen (es, c) =
  let pat = Pattern.[Pos (Fen (x, fs)); Neg (Fen (x, gs))] in
  Pattern.find pat c >>= fun (aff, c) ->
  Some (
      let c = Literal.Set.add (Pos (Fen (Affect.var aff x, Affect.feat_set aff fs))) c in
      Feat.Set.diff (Affect.feat_set aff fs) (Affect.feat_set aff gs) |> Feat.Set.elements
      |> List.map
           (fun f ->
             let z = Var.fresh () in
             (Var.Set.add z es,
              Literal.Set.add (Pos (Feat (Affect.var aff x, f, z))) c))
    )

let all : (string * (Conj.t -> Conj.disj option)) list = [
    (* FIXME: write the others *)
    "C-Feat-Abs",   c_feat_abs;
    "C-NEq-Refl",   c_neq_refl;
    "C-NSim-Refl",  c_nsim_refl;
    "S-Eq",         s_eq;
    "S-Feats",      s_feats;
    "S-Feats-Glob", s_feats_glob;
    "P-Feats",      p_feat;
    "R-NFeat",      r_nfeat;
    "R-NFen-Fen",   r_nfen_fen;
  ]
