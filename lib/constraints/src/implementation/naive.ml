open Constraints_common open Syntax

type conj = (Var.Set.t [@opaque]) * literal list
[@@deriving show { with_path = false }]

type t = conj

type disj = conj list
[@@deriving show { with_path = false }]

let true_ = (Var.Set.empty, [])

let c_feat_abs (_, c) =
  let (x, y, f) = Metavar.fresh3 () in
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Abs (x, f))] in
  OptionMonad.(Pattern.find pat c >>= fun _ -> Some [])

let c_neq_refl (_, c) =
  let x = Metavar.fresh () in
  let pat = Pattern.[Neg (Eq (x, x))] in
  OptionMonad.(Pattern.find pat c >>= fun _ -> Some [])

let c_nsim_refl (_, c) =
  let (x, fs) = Metavar.fresh2 () in
  let pat = Pattern.[Neg (Sim (x, fs, x))] in
  OptionMonad.(Pattern.find pat c >>= fun _ -> Some [])

let p_feat (es, c) =
  let (x, y, z, fs, f) = Metavar.fresh5 () in
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))] in
  OptionMonad.(
    Pattern.find
      ~pred:(fun aff -> not (Feat.Set.mem (Affect.feat aff f) (Affect.feat_set aff fs)))
      pat c
    >>= fun (aff, c) ->
    Some [
        es,
        [Pos (Sim (Affect.var aff x, Affect.feat_set aff fs, Affect.var aff y));
         Pos (Feat (Affect.var aff x, Affect.feat aff f, Affect.var aff z));
         Pos (Feat (Affect.var aff y, Affect.feat aff f, Affect.var aff z))] @ c
      ]
  )

let apply rule disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) conj ->
        match rule conj with
        | None -> (changes, conj :: disj)
        | Some disj' -> (true, disj' @ disj))
      (false, [])
      disj
  in
  if changes then Some disj else None

let apply_l rules disj =
  let (changes, disj) =
    List.fold_left
      (fun (changes, disj) rule ->
        match apply rule disj with
        | None -> (changes, disj)
        | Some disj -> (true, disj))
      (false, disj)
      rules
  in
  if changes then Some disj else None

let rules = [
    (* c_cycle *)
    c_feat_abs;
    (* c_feat_fen *)
    c_neq_refl;
    c_nsim_refl;
    (* s_eq *)
    (* ... *)
    p_feat;
    (* (fun _ -> assert false) *)
  ]

let rec normalize d =
  match apply_l rules d with
  | None -> d
  | Some d -> normalize d

let quantify_over x (e, c) = normalize [Var.Set.add x e, c]
let add l (e, c) = normalize [e, l :: c]

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
