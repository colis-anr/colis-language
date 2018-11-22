open Constraints_common open Syntax

type t = Var.Set.t * literal list
let true_ = (Var.Set.empty, [])

module Pattern = struct
  type metavar = int

  let fresh_metavar =
    let i = ref 0 in
    fun () -> incr i; !i

  type affect =
    (metavar * Var.t) list
    * (metavar * Feat.t) list
    * (metavar * Feat.Set.t) list

  let empty_affect = [], [], []

  type atom =
    | Eq of metavar * metavar
    | Feat of metavar * metavar * metavar
    | Abs of metavar * metavar
    | Fen of metavar * metavar
    | Sim of metavar * metavar * metavar

  let match_atom (pa : atom) (a : Syntax.atom) : affect list =
    match pa, a with
    | Eq (mx, my), Eq (x, y) ->
       [[mx, x; my, y], [], [];
        [mx, y; my, x], [], []]
    | Feat (mx, mf, my), Feat (x, f, y) ->
       [[mx, x; my, y], [mf, f], []]
    | Abs (mx, mf), Abs (x, f) ->
       [[mx, x], [mf, f], []]
    | Fen (mx, mfs), Fen (x, fs) ->
       [[mx, x], [], [mfs, fs]]
    | Sim (mx, mfs, my), Sim (x, fs, y) ->
       [[mx, x; my, y], [], [mfs, fs];
        [mx, y; my, x], [], [mfs, fs]]
    | _ ->
       []

  type literal =
    | Pos of atom
    | Neg of atom

  let match_literal (pl : literal) (l : Syntax.literal) : affect list =
    match pl, l with
    | Pos pa, Pos a -> match_atom pa a
    | Neg pa, Neg a -> match_atom pa a
    | _ -> []

  let rec match_literal_l (pl : literal) (ls : Syntax.literal list) : (affect * Syntax.literal list) list =
    match ls with
    | [] -> []
    | l :: ls ->
       List.map (fun affect -> (affect, ls)) (match_literal pl l)
       @ List.map (fun (affect, ls) -> (affect, l :: ls)) (match_literal_l pl ls)

  let rec match_ (pls : literal list) (ls : Syntax.literal list) : (affect * Syntax.literal list) list =
    match pls with
    | [] -> empty_affect
    | pl :: pls ->
       let affects = match_literal_l pl ls in
       List.map (fun (affect, rest) -> ) (*FIXME*)

  let find _pat _c = assert false
end

let c_neq_refl c =
  Pattern.(
    let pat =
      let x = fresh_metavar () in
      ([], [Neg (Eq (x, x))])
    in
    match find pat c with
    | None -> None
    | Some _ -> Some []
  )

let c_nsim_refl (_, c) =
  Pattern.(
    let pat =
      let x = fresh_metavar () in
      let fs = fresh_metavar () in
      [], [Neg (Sim (x, fs, x))]
    in
    match find pat c with
    | None -> None
    | Some _ -> Some []
  )

let apply rule d =
  let (changes, d) =
    List.fold_left
      (fun (changes, d) c ->
        match rule c with
        | None -> (changes, c :: d)
        | Some d' -> (true, d' @ d))
      (false, [])
      d
  in
  if changes then
    Some d
  else
    None

let apply_l rules d =
  let (changes, d) =
    List.fold_left
      (fun (changes, d) rule ->
        match apply rule d with
        | None -> (changes, d)
        | Some d -> (true, d))
      (false, d)
      rules
  in
  if changes then
    None
  else
    Some d

let rules = [
    c_neq_refl;
    c_nsim_refl;
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
