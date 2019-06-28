open SymbolicUtility

module IdMap = Env.IdMap

(******************************************************************************)
(*                                  true/false                                *)
(******************************************************************************)

let interp_true : context -> utility =
  fun _ ->
    return true

let interp_false : context -> utility =
  fun _ ->
    return false

(******************************************************************************)
(*                                     echo                                   *)
(******************************************************************************)

let interp_echo : context -> utility =
  fun ctx sta ->
  let str = String.concat " " ctx.args in
  let sta = print_stdout ~newline:true str sta in
  [sta, true]

(**************************************************************************)
(*                     update-alternatives                                *)
(**************************************************************************)

let interp_update_alternatives ctx =
  let name = "update-alternatives" in
  let rec aux = function
    | [] ->
      (* TODO: return error state *)
      unsupported ~utility:name "no sub-command found"
    | "--quiet" :: rem->
      fun st -> aux rem (print_utility_trace (name ^ ": ignored option --quiet") st)
    | arg :: _ ->
      unsupported ~utility:name ("unsupported argument: " ^ arg)
  in
  aux ctx.args

(**************************************************************************)
(*                                    dpkg                                *)
(**************************************************************************)

let interp_dpkg ctx =
  let utility = "dpkg" in
  let aux = function
    | ["-L"; _pkg_name] ->
      unsupported ~utility "support for -L not yet implemented"
    | "-L" :: _ ->
      unsupported ~utility "option -L expects exactly one argument"
    | ["--compare-versions"; _v1; _v2] ->
      unsupported ~utility "support for --compare-versions not yet implemented"
    | "--compare-versions" :: _ ->
      unsupported ~utility "option --compare-versions expects exactly two arguments"
    | [] ->
      (* TODO: return error state *)
      unsupported ~utility "no argument found"
    | arg :: _ ->
      unsupported ~utility ("unsupported argument: " ^ arg)
  in
  aux ctx.args


let register () =
  (* These calls can be moved to the modules that implement the utilities *)
  let register' (name, f) = register (module struct let name = name let interprete = f end) in
  List.iter register' [
    "true", interp_true;
    "false", interp_false;
    "echo", interp_echo;
    "update-alternatives", interp_update_alternatives;
    "dpkg", interp_dpkg;
  ];
  List.iter register [
      (module DpkgMaintscriptHelper) ;
      (module Mv);
      (module Mkdir);
      (module Rm) ;
      (module Test) ;
      (module Test.Bracket) ;
      (module Touch) ;
      (module Which) ;
      (module Which.Silent) ;
  ]
