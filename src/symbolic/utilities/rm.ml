open Format
open Colis_constraints
open Clause
open SymbolicUtility

let name = "rm"

let interp1 cwd arg : utility =
  let oq = Path.from_string arg in
  under_specifications @@
  match Path.split_last oq with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
    [error_case ~descr:"rm: invalid path ''"]
  | Some (_q, (Here|Up)) ->
    [error_case ~descr:"rm: cannot remove .. or ."]
  | Some (q, Down f) -> [
      success_case
        ~descr:(asprintf "rm %a: remove file" Path.pp oq)
        begin fun root root' ->
          let hintx = last_comp_as_hint ~root q in
          let hinty = Feat.to_string f in
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd oq y & ndir y
          & similar root root' cwd q x x'
          & sim x (Feat.Set.singleton f) x'
          & dir x' & abs x' f
        end;
      error_case
        ~descr:(asprintf "rm %a: target does not exist or is a directory" Path.pp oq)
        begin fun root root' ->
          let hinty = Feat.to_string f in
          exists ~hint:hinty @@ fun y ->
          maybe_resolve root cwd oq y
          & dir y
          & eq root root'
        end;
    ]

let interp1_r cwd arg : utility =
  let oq = Path.from_string arg in
  under_specifications @@
  match Path.split_last oq with
  (* FIXME: Here, I reuse the same programming scheme as in mkdir. *)
  (* FIXME: Shouldn't we factorize it in a combinator?             *)
  | None ->
    [error_case ~descr:"rm: invalid path ''"]
  | Some (_q, (Here|Up)) ->
    [error_case ~descr:"rm: cannot remove .. or ."]
  | Some (q, Down f) -> [
      success_case
        ~descr:(asprintf "rm -r %a: remove file or directory" Path.pp oq)
        begin fun root root' ->
          let hintx = last_comp_as_hint ~root q in
          let hinty = Feat.to_string f in
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd oq y
          & similar root root' cwd q x x'
          & sim x (Feat.Set.singleton f) x'
          & dir x' & abs x' f
        end;
      error_case
        ~descr:(asprintf "rm -r %a: target does not exist" Path.pp oq)
        begin fun root root' ->
          noresolve root cwd oq & eq root root'
        end;
    ]

let interprete recursive force ctx args =
  let rm = multiple_times ((if recursive then interp1_r else interp1) ctx.cwd) args in
  if force then uor rm (return true) else rm

let interprete ctx : utility =
  let recursive = Cmdliner.Arg.(value & flag & info ["r"; "R"; "recursive"]) in
  let force = Cmdliner.Arg.(value & flag & info ["f"; "force"]) in
  cmdliner_eval_utility
    ~utility:name
    Cmdliner.Term.(const interprete $ recursive $ force)
    ctx
