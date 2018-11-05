open Format
open Batteries
open Symbexec__Definitions
open Symbexec_printers

(* To compose constraints without too many parentheses *)
let (&) = Set.add

let on_constraints f fs = {fs with constraints=f fs.constraints}

(** {2 Variable substitution} *)

let subst_var src dst v =
  if v = src then dst else v

(** [subst_constraint src dst c] substitutes variable [src] with variable [dst] in
    constraint [c]. *)
let subst_constraint src dst =
  let subst_var = subst_var src dst in
  function
  | Bottom -> Bottom
  | Eq (x, y) -> Eq (subst_var x, subst_var y)
  | Feature (x, f, y) -> Feature (subst_var x, f, subst_var y)
  | Present (x, f) -> Present (subst_var x, f)
  | Absent (x, f) -> Absent (subst_var x, f)
  | Dir x -> Dir (subst_var x)
  | Reg x -> Reg (subst_var x)
  | Fence (x, fs) -> Fence (subst_var x, fs)
  | Similar (x, fs, y) -> Similar (subst_var x, fs, subst_var y)

(** [subst_constraints src dst cs] substitutes variable [src] with variable [dst] in
    constraint set [cs] *)
let subst_constraints src dst cs =
  Set.map (subst_constraint src dst) cs

let subst_path src dst p =
  let subst_pair (v, f) =
    subst_var src dst v, f
  in
  List.map subst_pair p

let subst_filesystem src dst fs =
  let root = subst_var src dst fs.root in
  let constraints = subst_constraints src dst fs.constraints in
  let cwd = subst_path src dst fs.cwd in
  {root; constraints; cwd}

(** {2 Trivial constraints} *)

(** Removes constraints that do not carry any information (i.e., tautology and reflexive
    equality) *)
let remove_trivial cs =
  let trivial = function
    | Eq (x, y) when x = y -> true
    | Similar (x, fs, y) when Set.is_empty fs -> true
    | _ -> false
  in
  Set.filter (fun c -> not (trivial c)) cs

(** {2 Contradictions (clashes} *)

(** The result of [child_sets features] maps each variable [x] to all variables [y] where
    [(x, _, y)] is in [features]. *)
let children features : (var, var Set.t) Map.t =
  let aux (x, f, y) acc =
    Map.modify_def Set.empty x (Set.add y) acc
  in
  Set.fold aux features Map.empty

(** The result of [child_sets features] maps each variable [x] to all variables [y] where
    [(x, _, y)] is in [features]. *)
let children_with_features features : (var, (feature, var) Map.t) Map.t =
  let aux (x, f, y) acc =
    Map.modify_def Map.empty x (Map.add f y) acc
  in
  Set.fold aux features Map.empty

(** Detect cycles in a set of features by adding descendants to the child_set *)
let has_cycle fs =
  let rec step descendants =
    (* Add for each var in [descendants] all descendants *)
    let descendants' =
      let add_grand_children (x:var) (ys:var Set.t) =
        let lookup y = Map.find_default Set.empty y descendants in
        Set.fold Set.union (Set.map lookup ys) Set.empty
      in
      Map.mapi add_grand_children descendants in
    let has_loop x ys = Set.mem x ys in
    if Map.exists has_loop descendants' then
      (* Loop detected *)
      true
    else if descendants = descendants' then
      (* No change anymore; saturated *)
      false
    else
      (* Continue with added descendants *)
      step descendants'
  in
  step (children fs)

(** Check for contradictory absence constraint to any feature *)
let has_present_absent fs cs =
  let presents = Set.filter_map (function Present (x, f) -> Some (x, f) | _ -> None) cs in
  let absents = Set.filter_map (function Absent (x, f) -> Some (x, f) | _ -> None) cs in
  not (Set.disjoint presents absents)
(* let test (x, f, y) =
 *   Set.mem (Absent (x, f)) cs
 * in
 * Set.exists test fs *)

(** Check for contradictory fence constraint to any feature *)
let has_feature_neg_fence fs cs =
  let test (x, f, y) =
    let is_neg_fence = function
      | Fence (x', fs) -> x' = x && not (Set.mem f fs)
      | _ -> false
    in
    Set.exists is_neg_fence cs
  in
  Set.exists test fs

(** Check for contradictory reg/dir constraints *)
let has_reg_dir cs =
  let test_reg x = function
    | Reg x' -> x' = x
    | _ -> false
  in
  let test_dir = function
    | Dir x -> Set.exists (test_reg x) cs
    | _ -> false
  in
  Set.exists test_dir cs

let features cs =
  let feature = function Feature (x, f, y) -> Some (x, f, y) | _ -> None in
  Set.filter_map feature cs

(** Check for contradictions in a constraint set *)
let has_contradiction cs =
  (* The triples [(x, f, y)] from all feature constraints  *)
  let fs = features cs in
  Set.mem Bottom cs ||
  has_cycle fs ||
  has_present_absent fs cs ||
  has_reg_dir cs

(** {2 Better printer for constraints} *)
let print_filesystem fmt fs =
  let ch = children_with_features (features fs.constraints) in
  let var_order v1 v2 = Tuple2.compare (v2=fs.root, v1) (v1=fs.root, v2) in
  let first_var = function
    | Bottom -> None
    | Eq (v, _) | Feature (v, _, _)
    | Present (v, _) | Absent (v, _)
    | Fence (v, _) | Similar (v, _, _)
    | Dir v | Reg v  -> Some v
  in
  let roots =
    let open Set in
    let parents = of_enum (Map.keys ch) in
    let children = Map.values ch |> Enum.map Map.values |> Enum.flatten |> Set.of_enum in
    diff parents children
  in
  let rec print_tree fmt v =
    let print_root_marker fmt =
      if v = fs.root then pp_print_string fmt "*"
    in
    let print_cwd_marker fmt =
      if v = path_target fs.cwd fs.root then pp_print_string fmt "*"
    in
    fprintf fmt "%t%a%t%a" print_root_marker print_var v print_cwd_marker
      print_children (Map.find_default Map.empty v ch)
  and print_sub_tree fmt (F f, v) =
    fprintf fmt "[%s]%a" f print_tree v
  and print_children fmt fvs =
    match Map.cardinal fvs with
    | 0 -> ()
    | 1 -> print_sub_tree fmt (List.hd @@ Map.bindings fvs)
    | _ ->
      let fvs = Map.bindings fvs |> List.sort Tuple2.compare in
      let pp_sep fmt () = fprintf fmt ",@ " in
      fprintf fmt "{@[@,%a@]}" (pp_print_list ~pp_sep print_sub_tree) fvs
  in
  let constraint_groups =
    List.filter (function Feature _ -> false | _ -> true) %>
    List.sort (fun c1 c2 -> compare (first_var c1) (first_var c2)) %>
    List.enum %> Enum.group first_var %>
    List.of_enum
  in
  let print_constraint_group fmt cs =
    let cs = List.of_enum cs |> List.sort compare in
    let pp_cstr_sep fmt () = fprintf fmt ",@ " in
    match first_var (List.hd cs) with
    | None -> fprintf fmt "@[%a@]" (pp_print_list ~pp_sep:pp_cstr_sep print_constraint) cs
    | Some v1 ->
      let print_cstr_rest fmt = function
        | Eq (_, v2) -> fprintf fmt "=%a" print_var v2
        | Present (_, f) -> fprintf fmt "[%a]↓" print_feature f
        | Absent (_, f) -> fprintf fmt "[%a]↑" print_feature f
        | Dir _ -> fprintf fmt "dir"
        | Reg _ -> fprintf fmt "reg"
        | Fence (_, fs) ->
          fprintf fmt "[%a]"
            (pp_print_list ~pp_sep:(pp_sep ",@ ") print_feature) (Set.to_list fs)
        | Similar (_, fs, y) ->
          fprintf fmt "≈%a[%a]" print_var y
            (pp_print_list ~pp_sep:(pp_sep ",@ ") print_feature) (Set.to_list fs)
        | _ -> ()
      in
      fprintf fmt "- @[%a:@ @[%a@]@]" print_var v1 (pp_print_list ~pp_sep:pp_cstr_sep print_cstr_rest) cs
  in
  let sep_groups fmt () = fprintf fmt "@\n" in
  Set.to_list roots |> List.sort var_order |> List.iter (fprintf fmt "- %a@\n" print_tree);
  Set.to_list fs.constraints |> constraint_groups |> pp_print_list ~pp_sep:sep_groups print_constraint_group fmt

(** {2 Propagation} *)

(** [select f cs] extracts data from one constraint in [cs] using [f], and returns the
    data together with all other constraints *)
let select f cs =
  let f' c =
    match f c with
    | Some x -> Some (c, x)
    | None -> None
  in
  let candidates = Set.filter_map f' cs in
  if Set.is_empty candidates
  then None
  else
    let c, x = Set.any candidates in
    Some (x, Set.remove c cs)

(** Substitutes variables used from equality constraints *)
let rec subst_eq fs =
  let eq = function
    | Eq (x, y) when x <> y ->
      Some (x, y)
    | _ -> None
  in
  match select eq fs.constraints with
  | Some ((x, y), _) ->
    subst_eq (subst_filesystem x y fs)
  | None -> fs

(** Add directory constraint for first variables from all feature constraints *)
let rec add_feature_dir cs =
  let feature = function
    | Feature (x, f, y)
      when not (Set.mem (Dir x) cs) ->
      Some (x, f, y)
    | _ -> None
  in
  match select feature cs with
  | Some ((x, f, y), _) ->
    let cs' = Dir x & cs in
    add_feature_dir cs'
  | None -> cs

(** Add presence constraints for second variables from all feature constraints *)
let rec add_feature_presence cs =
  let feature = function
    | Feature (x, f, y)
      when not (Set.mem (Present (x, f)) cs) ->
      Some (x, f, y)
    | _ -> None
  in
  match select feature cs with
  | Some ((x, f, y), _) ->
    let cs' = Present (x, f) & cs in
    add_feature_presence cs'
  | None -> cs

(** Add an equality [y1 = y2] if there are features [x[f]y1] and [x[f]y2] *)
let feature_functional cs =
  let rec aux seen cs =
    let feature = function
      | Feature (x, f, y)
        when not (Set.mem (x, f, y) seen) ->
        Some (x, f, y)
      | _ -> None
    in
    (* Select one feature constraint we haven't seen before *)
    match select feature cs with
    | Some ((x, f, y), cs') ->
      let seen' = (x, f, y) & seen in
      (* Get second operands of all feature constraints of the form [x[f]y'] *)
      let ys =
        let matching_feature = function
          | Feature (x', f', y')
            when x' = x && f' = f && y' <> y ->
            Some y'
          | _ -> None
        in
        Set.filter_map matching_feature cs' in
      (* Build equality constraints [y=y'] *)
      let cs' = Set.(map (fun y' -> Eq (y, y')) ys) in
      aux seen' (Set.union cs cs')
    | None -> cs
  in aux Set.empty cs

(** Saturate the constraint set, i.e., expand with one-step-inference

    TODO fence and similarity
*)
let saturate fs =
  (* printf "  === @[%a@]@\n" print_constraints cs; *)
  let run cs (descr, f) =
    let cs' = f cs in
    (* printf "- %s@\n" descr; *)
    (* let added = Set.diff cs' cs in
     * let removed = Set.diff cs cs' in
     * if not (Set.is_empty added) then
     *   printf "  +++ @[%a@]@\n" print_constraints added;
     * if not (Set.is_empty removed) then
     *   printf "  --- @[%a@]@\n" print_constraints removed;
     * printf "  === @[%a@]@\n" print_constraints cs'; *)
    cs'
  in
  let cleanup fs = fs |> subst_eq |> on_constraints remove_trivial in
  List.fold_left run fs [
    "cleanup", cleanup;
    "add_feature_dir", on_constraints add_feature_dir;
    "add_feature_presence", on_constraints add_feature_presence;
    "feature_functional", on_constraints feature_functional;
    "cleanup", cleanup;
  ]

(** Propagate the constraint set, i.e.,

    - remove trivial constraints,
    - check for contradictions,
    - and saturate. *)
let rec propagate fs =
  let fs = on_constraints remove_trivial fs in
  if has_contradiction fs.constraints then
    {fs with constraints=contradiction}
  else
    let fs' = saturate fs in
    if Set.equal fs'.constraints fs.constraints
    then fs
    else propagate fs'

let rec remove_from_features seen cs =
  let feature = function
    | Feature (x, f, y)
      when not (Set.mem (x, f, y) seen) ->
      Some (x, f, y)
    | _ -> None
  in
  match select feature cs with
  | Some ((x, f, y), _) ->
    let seen' = (x, f, y) & seen in
    let cs' = Set.(cs |> remove (Dir x) |> remove (Present (x, f))) in
    remove_from_features seen' cs'
  | None -> cs

(** Remove redundant information *)
let unpropagate cs =
  cs |> remove_from_features Set.empty
