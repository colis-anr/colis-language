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

let interp ctx recursive force = function
  | [] ->
    error ~msg:"rm: missing operand" ()
  | args ->
    Format.(
      eprintf "recursive = %B@\nforce = %B@\nargs = [%a]@\n@."
        recursive force
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") (fun fmt -> fprintf fmt "%S")) args
    );
    let rm = multiple_times ((if recursive then interp1_r else interp1) ctx.cwd) args in
    if force then uor rm (return true) else rm

let interp ctx : utility =
  let open CmdParser in
  let recursive = flag ["r"; "R"; "recursive"] in
  let force = flag ["f"; "force"] in
  eval ~utility:"rm" ctx (fun_ interp ctx $ recursive $ force)
