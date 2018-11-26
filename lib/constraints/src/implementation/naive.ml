open Constraints_common open Atom open Literal

module List = struct
  include List

  let rec map_filter f = function
    | [] -> []
    | h :: t ->
       match f h with
       | None -> map_filter f t
       | Some h' -> h' :: map_filter f t

  let remove x = filter ((<>) x)
end

type conj = Var.Set.t * Literal.Set.t
[@@deriving show { with_path = false }]

let compare_conj (es1, ls1) (es2, ls2) =
  match Var.Set.compare es1 es2 with
  | 0 -> Literal.Set.compare ls1 ls2
  | n -> n

let equal_conj c1 c2 = compare_conj c1 c2 = 0

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

type t = conj

type disj = conj list
[@@deriving show { with_path = false }]

let true_ = (Var.Set.empty, Literal.Set.empty)

let (x, y, z) = Metavar.fresh3 ()
let (f, g) = Metavar.fresh2 ()
let (fs, gs) = Metavar.fresh2 ()

module Rule = struct
  open OptionMonad

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
end

let apply_rule_on_conj ((name : string), (rule : conj -> disj option)) conj =
  match rule conj with
  | None -> None
  | Some [] ->
     (
       Log.debug (fun m -> m "Clash %s applied" name);
       Some []
     )
  | Some [conj'] ->
     if equal_conj conj' conj then
       None
     else
       (
         Log.debug (fun m -> m "Rule %s applied" name);
         Some [conj']
       )
  | Some disj' ->
     Log.debug (fun m -> m "Rule %s applied" name);
     assert (List.for_all ((<>) conj) disj');
     Some disj'

let apply_rule_on_disj rule disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) conj ->
        match apply_rule_on_conj rule conj with
        | None -> (changes, conj :: disj)
        | Some disj' -> (true, disj' @ disj))
      (false, [])
      disj
  in
  if changes then Some disj else None

let apply_rules_on_disj (rules : (string * (conj -> disj option)) list) (disj : disj) : disj option =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) rule ->
        match apply_rule_on_disj rule disj with
        | None -> (changes, disj)
        | Some disj -> (true, disj))
      (false, disj)
      rules
  in
  if changes then Some disj else None

let rules : (string * (conj -> disj option)) list = Rule.[
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

let rec normalize limit d =
  Log.debug (fun m -> m "Normalizing:@\n%a" pp_disj d);
  assert (limit >= 0);
  match apply_rules_on_disj rules d with
  | None ->
     Log.debug (fun m -> m "Normal form reached");
     d
  | Some d ->
     normalize (limit-1) d

let normalize ?(limit=10) disj =
  normalize limit disj

let quantify_over x (e, c) = normalize [Var.Set.add x e, c]
let add l (e, c) = normalize [e, Literal.Set.add l c]

let eq x y = add (Pos (Eq (x, y)))
let neq x y = add (Neg (Eq (x, y)))
let feat x f y = add (Pos (Feat (x, f, y)))
let nfeat x f y = add (Neg (Feat (x, f, y)))
let abs x f = add (Pos (Abs (x, f)))
let nabs x f = add (Neg (Abs (x, f)))
let reg x = add (Pos (Reg x))
let nreg x = add (Neg (Reg x))
let dir x = add (Pos (Reg x))
let ndir x = add (Neg (Reg x))
let fen x fs = add (Pos (Fen (x, fs)))
let nfen x fs = add (Neg (Fen (x, fs)))
let sim x fs y = add (Pos (Sim (x, fs, y)))
let nsim x fs y = add (Neg (Sim (x, fs, y)))
