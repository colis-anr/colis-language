open Format
open Batteries
open Semantics__Buffers
open Symbexec__Definitions

let pp_sep sep fmt () = fprintf fmt sep

let print_var fmt (V s) =
  fprintf fmt "%s" s

let print_feature fmt (F s) =
  fprintf fmt "%s" s

let print_constraint fmt c =
  match c with
  | Bottom -> fprintf fmt "⊥"
  | Eq (v1, v2) -> fprintf fmt "%a = %a" print_var v1 print_var v2
  | Feature (v1, f, v2) -> fprintf fmt "%a[%a]%a" print_var v1 print_feature f print_var v2
  | Present (x, f) -> fprintf fmt "%a[%a]↓" print_var x print_feature f
  | Absent (x, f) -> fprintf fmt "%a[%a]↑" print_var x print_feature f
  | Dir x -> fprintf fmt "dir(%a)" print_var x
  | Reg x -> fprintf fmt "reg(%a)" print_var x
  | Fence (x, fs) ->
    fprintf fmt "%a[%a]" print_var x
      (pp_print_list ~pp_sep:(pp_sep ",@ ") print_feature) (Set.to_list fs)
  | Similar (x, fs, y) ->
    fprintf fmt "%a≈%a[%a]" print_var x print_var y
      (pp_print_list ~pp_sep:(pp_sep ",@ ") print_feature) (Set.to_list fs)

let print_constraints fmt cs =
  let pp_sep fmt () = fprintf fmt " ∧@ " in
  fprintf fmt "@[%a@]"
    (pp_print_list ~pp_sep print_constraint)
    (Set.to_list cs)

let print_path fmt (np, root) =
  let pp_pair fmt (x, f) = fprintf fmt "[%a]%a" print_feature f print_var x in
  fprintf fmt "%a%a"
    print_var root
    (pp_print_list ~pp_sep:(pp_sep "") pp_pair) (List.rev np)

let print_state_not_filesystem fmt sta =
  if not (stdin_is_empty sta.stdin) then
    fprintf fmt "@\nstdin: %a" (pp_print_list ~pp_sep:(pp_sep "@\n") pp_print_string) sta.stdin;
  if not (stdout_is_empty sta.stdout) then begin
    let lines = List.rev sta.stdout in
    let lines = if List.last lines = "" then List.take (List.length lines - 1) lines else lines in
    fprintf fmt "@\nstdout:@\n  @[";
    pp_print_list ~pp_sep:pp_force_newline (fun fmt -> fprintf fmt "%s") fmt lines;
    fprintf fmt "@]"
  end

let print_state fmt sta =
  fprintf fmt "result: %b@\npath: %a@\nconstraints:@\n  @[%a@]@"
    sta.result print_path (sta.filesystem.cwd, sta.filesystem.root) print_constraints sta.filesystem.constraints;
  print_state_not_filesystem fmt sta
