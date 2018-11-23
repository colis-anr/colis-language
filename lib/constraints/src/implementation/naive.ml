open Constraints_common open Syntax

module List = struct
  include List

  let remove x = filter ((<>) x)
end

type conj = Var.t list * literal list
[@@deriving show { with_path = false }]

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

let rec replace_var_in_literal_list x y = function
  | [] -> []
  | l :: ls -> replace_var_in_literal x y l :: replace_var_in_literal_list x y ls

type t = conj

type disj = conj list
[@@deriving show { with_path = false }]

let true_ = ([], [])

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

let s_feats (es, c) =
  let (x, y, z, f) = Metavar.fresh4 () in
  let pat = Pattern.[Pos (Feat (x, f, y)); Pos (Feat (x, f, z))] in
  OptionMonad.(
    Pattern.find
      ~pred:(fun aff -> List.mem (Affect.var aff x) es &&
                          not (Var.equal (Affect.var aff y) (Affect.var aff z)))
      pat c
    >>= fun (aff, c) ->
    Some [List.remove (Affect.var aff x) es,
          Pos (Feat (Affect.var aff x, Affect.feat aff f, Affect.var aff y)) :: replace_var_in_literal_list (Affect.var aff z) (Affect.var aff y) c]
  )

let p_feat (es, c) =
  let (x, y, z, fs, f) = Metavar.fresh5 () in
  let pat = Pattern.[Pos (Sim (x, fs, y)); Pos (Feat (x, f, z))] in
  OptionMonad.(
    Pattern.find
      ~pred:(fun aff -> not (Feat.Set.mem (Affect.feat aff f) (Affect.feat_set aff fs)))
      pat c
    >>= fun (aff, c) ->
    let a = Pos (Feat (Affect.var aff y, Affect.feat aff f, Affect.var aff z)) in
    if List.mem a c then
      None (* FIXME: not satisfying, I don't want to check this myself *)
    else
      Some [
          es,
          [Pos (Sim (Affect.var aff x, Affect.feat_set aff fs, Affect.var aff y));
           Pos (Feat (Affect.var aff x, Affect.feat aff f, Affect.var aff z));
           a] @ c
      ]
  )

let r_nfeat (es, c) =
  let (x, y, f) = Metavar.fresh3 () in
  let pat = Pattern.[Neg (Feat (x, f, y))] in
  OptionMonad.(
    Pattern.find pat c >>= fun (aff, c) ->
    Some [
        (es, Pos (Abs (Affect.var aff x, Affect.feat aff f)):: c);
        let z = Var.fresh () in
        (z::es, [Pos (Feat (Affect.var aff x, Affect.feat aff f, z)); Neg (Sim (Affect.var aff x, Feat.Set.empty, z))] @ c)
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
    (* FIXME: write the others *)
    c_feat_abs;
    c_neq_refl;
    c_nsim_refl;
    s_feats;
    p_feat;
    r_nfeat;
  ]

let rec normalize ?(i=0) d =
  assert (i < 200);
  match apply_l rules d with
  | None -> d
  | Some d -> normalize ~i:(i+1) d

let quantify_over x (e, c) = normalize [x::e, c]
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
