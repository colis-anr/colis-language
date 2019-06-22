open Format
open Constraints
open Clause
open SymbolicUtility

let interp1 cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
  let oq = Path.from_string arg in
  match Path.split_last oq with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
    failure ~error_message:"rm: invalid path ''" ()
  | Some (_q, (Here|Up)) ->
    failure ~error_message:"rm: cannot remove .. or ." ()
  | Some (q, Down f) ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      success_case
        ~descr:(asprintf "rm %a: remove file" Path.pp oq)
        begin
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd oq y & ndir y
          & similar root root' cwd q x x'
          & sim x (Feat.Set.singleton f) x'
          & dir x' & abs x' f
        end;
      error_case
        ~descr:(asprintf "rm %a: target is a directory" Path.pp oq)
        begin
          exists ~hint:hinty @@ fun y ->
          resolve root cwd oq y & dir y
          & eq root root'
        end;
      error_case
        ~descr:(asprintf "rm %a: target does not exist" Path.pp oq)
        begin
          noresolve root cwd oq & eq root root'
        end;
    ]

let interp1_r cwd arg : utility =
  under_specifications @@ fun ~root ~root' ->
  let oq = Path.from_string arg in
  match Path.split_last oq with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
    failure ~error_message:"rm: invalid path ''" ()
  | Some (_q, (Here|Up)) ->
    failure ~error_message:"rm: cannot remove .. or ." ()
  | Some (q, Down f) ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      success_case
        ~descr:(asprintf "rm -r %a: remove file or directory" Path.pp oq)
        begin
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd q y & ndir y
          & similar root root' cwd q x x'
          & sim x (Feat.Set.singleton f) x'
          & dir x' & fen x' (Feat.Set.singleton f)
        end;
      error_case
        ~descr:(asprintf "rm -r %a: target does not exist" Path.pp oq)
        begin
          exists ~hint:hinty @@ fun y ->
          noresolve root cwd q & eq root root'
        end;
    ]

let interp_r cwd args : utility =
  match args with
  | [] -> error ~msg:"rm: missing operand" ()
  | [arg] -> interp1_r cwd arg
  | args -> multiple_times (interp1_r cwd) args

let interp ctx : utility =
  let e = ref None in
  let r = ref false in
  let f = ref false in
  let args = ref [] in
  List.iter
    (function
      | "-r" | "-R" -> r := true
      | "-f" -> f := true
      | "-rf" | "-fr" | "-Rf" | "-fR" -> r := true; f := true
      | arg ->
        if String.length arg > 0 && arg.[0] = '-' && !e = None then
          e := Some arg
        else
          args := arg :: !args)
    ctx.args;
  let args = List.rev !args in
  match !e with
  | Some arg -> unsupported ~utility:"rm" ("unknown argument: " ^ arg)
  | None ->
    if args = [] then
      error ~msg:"rm: missing operand" ()
    else
      let rm =
        if !r then
          interp_r ctx.cwd args
        else
          multiple_times (interp1 ctx.cwd) args
      in
      if !f then
        uor rm (return true)
      else
        rm
