open Format
open Colis_constraints
open Clause
open SymbolicUtility

let name = "touch"

let interp_touch1 cwd path_str : utility =
  (* FIXME: we can merge two cases here (parent path does not resolve & parent
     path isn't a directory) *)
  under_specifications @@ fun ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None -> (* `touch ''` *)
    failure ~error_message:"cannot touch '': No such file or directory" ~root ~root'
  | Some (q, (Up | Here)) ->
    let hint = last_comp_as_hint ~root p in [
      success_case
        ~descr:(asprintf "touch %a: path resolves" Path.pp p)
        begin
          exists ?hint @@ fun y ->
          resolve root cwd p y &
          eq root root'
        end;
      error_case
        ~descr:(asprintf "touch %a: path does not resolve" Path.pp p)
        begin
          noresolve root cwd p &
          eq root root'
        end
    ]
  | Some (q, Down f) ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      success_case
        ~descr:(asprintf "touch %a: path resolves" Path.pp p)
        begin
          exists ~hint:hinty @@ fun y ->
          resolve root cwd p y &
          eq root root'
        end;
      success_case
        ~descr:(asprintf "touch %a: create file" Path.pp p)
        begin
          exists3 ?hint1:hintx ?hint2:hintx ~hint3:hinty @@ fun x x' y' ->
          resolve root cwd q x &
          dir x &
          abs x f &
          similar root root' cwd q x x' &
          sim x (Feat.Set.singleton f) x' &
          dir x' &
          feat x' f y' &
          reg y'
        end;
      error_case
        ~descr:(asprintf "touch %a: parent path does not resolve or resolves to dir" Path.pp p)
        begin
          exists ?hint:hintx @@ fun x ->
          maybe_resolve root cwd q x
          & ndir x
          & eq root root'
        end
    ]

let interprete ctx args =
  multiple_times (interp_touch1 ctx.cwd) args

let interprete ctx : utility =
  cmdliner_eval_utility
    ~utility:name
    Cmdliner.Term.(const interprete)
    ctx
