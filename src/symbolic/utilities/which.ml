open Format
open Colis_constraints
open Clause
open SymbolicUtility.ConstraintsCompatibility
open Semantics__Result
open Semantics__Buffers

module Silent = struct
  let name = "silent-which"

  let interprete _ctx = function
    | [p] ->
      specification_cases [
        success_case
          ~descr:(asprintf "silent-which '%s': assuming command is found" p)
          noop
        ;
        error_case
          ~descr:(asprintf "silent-which '%s': assuming command is not found" p)
          noop
      ]
    | _ ->
      incomplete ~utility:"silent-which" "more than one argument"

  let interprete ctx : utility =
    cmdliner_eval_utility
      ~utility:name
      Cmdliner.Term.(const interprete)
      ctx
end

let name = "which"

let interp_test_regular_and_x cwd path_str : utility =
  let p = Path.from_string path_str in
  specification_cases [
    success_case
      ~descr:(asprintf "which '%a': path resolves to a regular executable (overapprox to -f)" Path.pp p)
      ~stdout:Stdout.(empty |> output (asprintf "%a" Path.pp p) |> newline)
      begin fun root root' ->
        let hintx = last_comp_as_hint ~root p in
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & reg x & (* no way to constraint "x" mode *)
        eq root root'
      end;
    error_case
      ~descr:(asprintf "which '%a': path does not resolve" Path.pp p)
      begin fun root root' ->
        noresolve root cwd p &
        eq root root'
      end;
    error_case
      ~descr:(asprintf "which '%a': path resolves but not to regular executable)" Path.pp p)
      begin fun root root' ->
        let hintx = last_comp_as_hint ~root p in
        exists ?hint:hintx @@ fun x ->
        resolve root cwd p x & (* no way to constraint no "x" mode *)
        eq root root'
      end;
  ]


let rec search_as_which_in_path cwd (path:string list) arg : utility =
  match path with
  | [] ->
    specification_cases [
      error_case
        ~descr:(asprintf "which: `%s` not found in PATH" arg)
        noop
    ]
  | p :: rem ->
    let u1 = interp_test_regular_and_x cwd (p ^ "/" ^ arg) in
    let u2 = search_as_which_in_path cwd rem arg in
    fun st ->
      List.flatten
        (List.map
           (function (s1,Ok b1) as x -> if b1 then [x] else u2 s1 | (_,Incomplete) as x -> [x])
           (u1 st))

let search_as_which cwd (path:string list) arg : utility =
  match Path.from_string arg with
  | Abs _ -> interp_test_regular_and_x cwd arg
  | Rel [] -> assert false
  | Rel [_] -> search_as_which_in_path cwd path arg
  | Rel r ->
    fun st ->
      let a = Path.concat cwd (Rel r) in
      interp_test_regular_and_x cwd ("/" ^ Path.rel_to_string a) st

let interprete all ctx args : utility =
  if all then incomplete ~utility:name "option `-a`" else
  (* FIXME     let path = String.split_on_char ':' (IdMap.find "PATH" ctx.env) in *)
  let path = [ "/usr/sbin" ; "/usr/bin" ; "/sbin" ; "/bin" (* ; "/usr/games" *) ] in
  let rec aux args =
    match args with
    | [] -> assert false
    | [a] -> search_as_which ctx.cwd path a
    | a :: rem ->
      uand (search_as_which ctx.cwd path a) (aux rem) (* FIXME: uand ??? *)
  in
  aux args

let interprete ctx : utility =
  let all = Cmdliner.Arg.(value & flag & info ["a"; "all"]) in
  cmdliner_eval_utility
    ~utility:name
    Cmdliner.Term.(const interprete $ all)
    ctx
