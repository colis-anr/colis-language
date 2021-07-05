open Format
open Colis_constraints
open SymbolicUtility.ConstraintsCompatibility

let name = "test"

(******************************************************************************)
(*                                     test                                   *)
(******************************************************************************)

let interp_test_parse_error args : utility =
  specification_cases [
    let descr = "test: parse error in `" ^ (String.concat " " args) ^ "`" in
    error_case ~descr noop
  ]

let interp_test_empty () : utility =
  specification_cases [
    let descr = "test: empty expression" in
    error_case ~descr noop
  ]

let interp_test_e cwd path_str : utility =
  let p = Path.from_string path_str in
  specification_cases [
    success_case
      ~descr:(asprintf "test -e %a: path resolves" Path.pp p)
      begin fun root root' ->
        exists @@ fun x ->
        resolve root cwd p x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -e %a: path does not resolve" Path.pp p)
      begin fun root root' ->
        noresolve root cwd p &
        eq root root'
      end;
  ]

(* obsolete
let interp_test_d cwd path_str : utility =
  specification_cases @@ fun ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -d %a: path resolves to a dir" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & dir x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -d %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -d %a: path resolves but not to a dir" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & ndir x &
        eq root root'
      end;
  ]

let interp_test_f cwd path_str : utility =
  specification_cases @@ fun ~root ~root' ->
  let p = Path.from_string path_str in
  let hintx = last_comp_as_hint ~root p in [
    success_case
      ~descr:(asprintf "test -f %a: path resolves to a regular file" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & reg x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -f %a: path does not resolve" Path.pp p)
      begin
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test -f %a: path resolves but not to a regular file" Path.pp p)
      begin
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & nreg x &
        eq root root'
      end;
  ]
 *)

let interp_test_file_type ~attr is_type is_ntype cwd path_str : utility =
  let strip_path = Path.strip_trailing_slashes path_str in
  let sp = Path.from_string strip_path in
  let p = Path.from_string path_str in
  let ok_case = success_case
      ~descr:(asprintf "test -%s %a: path resolves to file of type '%s'" attr Path.pp p attr)
      begin fun root root' ->
        exists @@ fun x ->
        resolve root cwd sp x & is_type x &
        eq root root'
      end in
  let err_case = 
    error_case
      ~descr:(asprintf "test -%s %a: path does not resolve or to file of type other than '%s'" attr Path.pp p attr)
      begin fun root root' ->
        exists @@ fun x ->
        maybe_resolve root cwd sp x
        & is_ntype x
        & eq root root'
      end in
  let is_dir = (is_type = dir) || (is_ntype = nreg) (* TODO: fill *)
  in
    specification_cases @@
    match Path.split_last p with
    | None ->  
      [ error_case ~descr:"test: invalid path ''" noop ]
    | Some(_q, (Here|Up)) ->
      if is_dir then
        [ ok_case; err_case; ]
      else
        [ error_case ~descr:"test: invalid path to file /. or /.." noop ]
    | Some(_q, (Down f)) ->
    if String.equal "" (Colis_constraints_common.Feat.to_string f)
    then (if is_dir then
        [ ok_case; err_case; ]
      else
        [ error_case ~descr:"test: path to file ends with / " noop ]
      )
    else
      [ ok_case; err_case; ]

let interp_test_attribute ~attr cwd path_str : utility =
  let p = Path.from_string path_str in
  specification_cases [
    success_case
      ~descr:(asprintf "test '%a': path resolves, attribute -%s OK (overapprox to -e)"
                Path.pp p attr)
      begin fun root root' ->
        exists @@ fun x ->
        resolve root cwd p x &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "test '%a': path does not resolve or resolves but attribute -%s not OK (overapprox to -e)" Path.pp p attr)
      noop
  ]

let interp_test_n str : utility =
  specification_cases @@
  if str = "" then
    [
      error_case
        ~descr:(asprintf "test -n '%s': string is empty" str)
        noop
    ]
  else
    [
      success_case
        ~descr:(asprintf "test -n '%s': string is non-empty" str)
        noop
    ]

let interp_test_z str : utility =
  specification_cases @@
  if str = "" then
    [
      success_case
        ~descr:(asprintf "test -z '%s': string is empty" str)
        noop
    ]
  else
    [
      error_case
        ~descr:(asprintf "test -z '%s': string is non-empty" str)
        noop
    ]

let interp_test_string_equal s1 s2 : utility =
  specification_cases @@
  if s1 = s2 then
    [
      success_case
        ~descr:(asprintf "test '%s' = '%s': strings are equal" s1 s2)
        noop
    ]
  else
    [
      error_case
        ~descr:(asprintf "test '%s' = '%s': string are not equal" s1 s2)
        noop
    ]

let interp_test_string_notequal s1 s2 : utility =
  specification_cases @@
  if s1 <> s2 then
    [
      success_case
        ~descr:(asprintf "test '%s' != '%s': strings are not equal" s1 s2)
        noop
    ]
  else
    [
      error_case
        ~descr:(asprintf "test '%s' != '%s': string are equal" s1 s2)
        noop
    ]

let rec interp_test_expr cwd e : utility =
  let name = "test" in
  Morsmall_utilities.TestParser.(
  match e with
  | Unary("-e",arg) -> interp_test_e cwd arg
  | Unary("-d",arg) -> interp_test_file_type ~attr:"d" dir ndir cwd arg
  | Unary("-f",arg) -> interp_test_file_type ~attr:"f" reg nreg cwd arg
  | Unary("-b",arg) -> interp_test_file_type ~attr:"b" block nblock cwd arg
  | Unary("-c",arg) -> interp_test_file_type ~attr:"c" char nchar cwd arg
  | Unary("-p",arg) -> interp_test_file_type ~attr:"p" pipe npipe cwd arg
  | Unary("-S",arg) -> interp_test_file_type ~attr:"S" sock nsock cwd arg
  | Unary("-h",arg) -> interp_test_file_type ~attr:"h" symlink nsymlink cwd arg
  | Unary("-L",arg) -> interp_test_file_type ~attr:"L" symlink nsymlink cwd arg
  | Unary("-G",arg) -> interp_test_attribute ~attr:"G" cwd arg
  | Unary("-O",arg) -> interp_test_attribute ~attr:"O" cwd arg
  | Unary("-g",arg) -> interp_test_attribute ~attr:"g" cwd arg
  | Unary("-k",arg) -> interp_test_attribute ~attr:"k" cwd arg
  | Unary("-r",arg) -> interp_test_attribute ~attr:"r" cwd arg
  | Unary("-s",arg) -> interp_test_attribute ~attr:"s" cwd arg
  | Unary("-u",arg) -> interp_test_attribute ~attr:"u" cwd arg
  | Unary("-w",arg) -> interp_test_attribute ~attr:"w" cwd arg
  | Unary("-x",arg) -> interp_test_attribute ~attr:"x" cwd arg
  | Unary("-n",arg) -> interp_test_n arg
  | Unary("-z",arg) -> interp_test_z arg
  | Binary ("=",a1,a2) -> interp_test_string_equal a1 a2
  | Binary ("!=",a1,a2) -> interp_test_string_notequal a1 a2
  | Binary ("-eq"|"-ne"|"-gt"|"-ge"|"-lt"|"-le" as opt,_,_) ->
    incomplete ~utility:name ("option "^opt)
  | Unary(op,_) ->
     (* The Unary cases above are complete, everything else is an error *)
     error ~utility:name ("unsupported unary operator: " ^ op)
  | And(e1,e2) ->
     uand (interp_test_expr cwd e1) (interp_test_expr cwd e2)
  | Or(e1,e2) ->
     uor (interp_test_expr cwd e1) (interp_test_expr cwd e2)
  | Not(e1) -> uneg (interp_test_expr cwd e1)
  | Binary (op,_e1,_e2) ->
    (* The Binary cases above are complete, everything else is an error *)
     error ~utility:name ("unknown binary operator: " ^ op)
  | Single arg ->
    incomplete ~utility:name ("unsupported single argument: " ^ arg))

let interpret ~bracket ctx : utility =
  match Morsmall_utilities.TestParser.parse ~bracket ctx.args with
  | None -> interp_test_empty ()
  | Some e -> interp_test_expr ctx.cwd e
  | exception Morsmall_utilities.TestParser.Parse_error ->
     interp_test_parse_error ctx.args

let interprete ctx = interpret ~bracket:false ctx

module Bracket = struct
  let name = "["
  let interprete ctx = interpret ~bracket:true ctx
end
