open Format
open Colis_constraints
open Clause
open SymbolicUtility.Symbolic

let name = "mv"

(*  Specification of 'rename()' system call
 *    used by 'mv'
 *)
let interp_rename ctx src dstpath : utility =
  let qsrc = Path.from_string src in
  let qdst = Path.from_string dstpath in
  specification_cases @@
    match Path.split_last qsrc, Path.split_last qdst with
    | (None, _) ->
      [error_case ~descr:"mv: invalid source path ''" noop]
    | (_, None) ->
      [error_case ~descr:"mv: invalid destination path ''" noop]
    | (Some (_, (Here|Up)), _) | (_, Some(_, (Here|Up))) ->
      [error_case ~descr:"mv: paths end in . or .." noop]
    | (Some (qs, Down fs), Some (qd, Down fd)) ->
       let unconditional_cases = [
           error_case
             ~descr:(asprintf "mv/rename: old '%s' does not resolve" src)
             begin fun root root' ->
               noresolve root ctx.cwd qsrc
               & eq root root'
             end;
           error_case
             ~descr:(asprintf "mv/rename: no path to new '%s'" dstpath)
             begin fun root root' ->
               noresolve root ctx.cwd qd
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: same file")
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintyd = last_comp_as_hint ~root qdst in
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys
               & resolve root ctx.cwd qdst yd
               & eq ys yd
               & eq root root'
             end;
           error_case
             ~descr:(asprintf "mv/rename: old '%s' is a directory, new '%s' is not"
                       src dstpath)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintyd = last_comp_as_hint ~root qdst in
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qdst yd & ndir yd
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: old '%s' is directory, new '%s' is an empty directory"
                       src dstpath)
             begin fun root root' ->
               let hintxs = last_comp_as_hint ~root qs in
               let hintys = last_comp_as_hint ~root qsrc in
               let hintxd = last_comp_as_hint ~root qd in
               let hintyd = last_comp_as_hint ~root qdst in
               exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
               exists2 ?hint1:hintyd ?hint2:None @@ fun yd xd' ->
               exists3 ?hint1:None ?hint2:hintxs ?hint3:hintxd @@ fun ri xsi xdi ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qdst yd & dir yd
               & neq ys yd & fen yd Feat.Set.empty
               & similar root ri ctx.cwd qs xs xsi
               & sim xs (Feat.Set.singleton fs) xsi
               & abs xsi fs
               & similar ri root' ctx.cwd qd xdi xd'
               & sim xdi (Feat.Set.singleton fd) xd'
               & feat xd' fd ys
             end;
           error_case
             ~descr:(asprintf "mv/rename: old '%s' is directory, '%s' is not an empty directory"
                       src dstpath)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintyd = last_comp_as_hint ~root qdst in
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qdst yd & dir yd
               & neq ys yd & nfen yd Feat.Set.empty
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: old '%s' is file, new '%s' is absent"
                       src dstpath)
             begin fun root root' ->
               let hintxs = last_comp_as_hint ~root qs in
               let hintys = last_comp_as_hint ~root qsrc in
               let hintxd = last_comp_as_hint ~root qd in
               exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
               exists2 ?hint1:hintxd ?hint2:None @@ fun xd xd' ->
               exists3 ?hint1:None ?hint2:hintxs ?hint3:hintxd @@ fun ri xsi xdi ->
               resolve root ctx.cwd qsrc ys & ndir ys
               & resolve root ctx.cwd qd xd
               & abs xd fd (* first case *)
               & similar root ri ctx.cwd qs xs xsi
               & sim xs (Feat.Set.singleton fs) xsi
               & abs xsi fs
               & similar ri root' ctx.cwd qd xdi xd'
               & sim xdi (Feat.Set.singleton fd) xd'
               & feat xd' fd ys
             end;
           success_case
             ~descr:(asprintf "mv/rename: old '%s' is file, new '%s' is not directory"
                       src dstpath)
             begin fun root root' ->
              let hintxs = last_comp_as_hint ~root qs in
              let hintys = last_comp_as_hint ~root qsrc in
              let hintxd = last_comp_as_hint ~root qd in
              let hintyd = last_comp_as_hint ~root qdst in
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintxd ?hint2:hintyd ?hint3:None @@ fun xd yd xd' ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi xdi ->
              resolve root ctx.cwd qsrc ys & ndir ys
              & resolve root ctx.cwd qd xd
              & feat xd fd yd & neq ys yd & ndir yd  (* second case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qd xdi xd'
              & sim xdi (Feat.Set.singleton fs) xd'
              & feat xd' fd ys
            end;
           error_case
             ~descr:(asprintf "mv/rename: old '%s' is a file, new '%s' is a directory"
                       src dstpath)
            begin fun root root' ->
              let hintys = last_comp_as_hint ~root qsrc in
              let hintyd = last_comp_as_hint ~root qdst in
              exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
              resolve root ctx.cwd qsrc ys & ndir ys
              & resolve root ctx.cwd qdst yd & dir yd
              & eq root root'
            end
         ]
       in
       let norm_qsrc = Path.normalize ~cwd:ctx.cwd qsrc in
       let norm_qdst = Path.normalize ~cwd:ctx.cwd qdst in
       let b_ancestor = Path.check_prefix norm_qsrc norm_qdst in
       let ancestor_case =
         if b_ancestor then
           [
           error_case
             ~descr:(asprintf "mv/rename: old '%s' is ancestor of new '%s'"
                     src dstpath)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintxd = last_comp_as_hint ~root qd in
               exists2 ?hint1:hintys ?hint2:hintxd @@ fun ys xd ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qd xd
               & eq root root'
             end
           ]
         else []
       in
       let b_slash = (String.length dstpath) >
                       (String.length (Path.strip_trailing_slashes dstpath)) in
       let slash_case =
         if b_slash then
           [
             error_case
               ~descr:(asprintf "old '%s' is directory, new '%s' is absent and slash"
                         src dstpath)
               begin fun root root' ->
                 let hintys = last_comp_as_hint ~root qsrc in
                 let hintxd = last_comp_as_hint ~root qd in
                 exists2 ?hint1:hintys ?hint2:hintxd @@ fun ys xd ->
                 resolve root ctx.cwd qsrc ys & dir ys
                 & resolve root ctx.cwd qd xd
                 & abs xd fd
                 & eq root root'
               end
           ]
         else
           [
             success_case
               ~descr:(asprintf "old '%s' is directory, new '%s' is absent"
                     src dstpath)
               begin fun root root' ->
                 let hintxs = last_comp_as_hint ~root qs in
                 let hintys = last_comp_as_hint ~root qsrc in
                 let hintxd = last_comp_as_hint ~root qd in
                 exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
                 exists2 ?hint1:hintxd ?hint2:None @@ fun xd xd' ->
                 exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi xdi ->
                 resolve root ctx.cwd qsrc ys & dir ys
                 & resolve root ctx.cwd qd xd
                 & abs xd fd
                 & similar root ri ctx.cwd qs xs xsi
                 & sim xs (Feat.Set.singleton fs) xsi
                 & abs xsi fs
                 & similar ri root' ctx.cwd qd xdi xd'
                 & sim xdi (Feat.Set.singleton fs) xd'
                 & feat xd' fd ys
               end
           ]
       in
       List.concat [ unconditional_cases; ancestor_case ; slash_case ]


let interp_mv2dir ctx dst src : utility =
  (* Assume: dst resolves to an existing directory *)
  let qsrc = Path.from_string src in
  match Path.split_last qsrc with
  | None ->
     error ~utility:"mv" "invalid source path ''"
  | Some (_, (Here|Up)) ->
     error ~utility:"mv" "source path ends in . or .."
  | Some (_, Down fs) ->
     let stripdst = Path.strip_trailing_slashes dst in
     let dstpath = String.concat "/" [stripdst; (Feat.to_string fs)] in
     interp_rename ctx src dstpath


let interprete ctx : utility =
  let e = ref None in
  let i = ref false in
  let args_rev = ref [] in
  List.iter
    (function
      | "-i" | "-fi" -> i := true
      | "-f" | "-if" -> () (* By default behaviour *)
      | arg ->
        if String.length arg > 0 && arg.[0] = '-' && !e = None then
          e := Some arg
        else
          args_rev := arg :: !args_rev)
    ctx.args;
  if !i then
    error ~utility:"mv" "option `-i` forbidden"
  else
    match !e with
    | Some arg -> error ~utility:"mv" ("unknown argument: " ^ arg)
    | None -> (
      let args = List.rev !args_rev in
      match args with
      | [] -> error ~utility:"mv" "missing operand"
      | [_arg] -> error ~utility:"mv" "not enough arguments"
      | [src; dst] ->
        (if_then_else
          (call "test" ctx ["-d"; dst])
          (interp_mv2dir ctx dst src)
          (interp_rename ctx src dst)
        )
      | _ ->
        (* More than two arguments, then mv in directory *)
        let dir = List.hd !args_rev in
        let srcs = List.tl !args_rev in
        (if_then_else
           (call "test" ctx ["-d"; dir])
           (multiple_times (interp_mv2dir ctx dir) (List.rev srcs))
           (error ~utility:"mv" "last argument not a directory")
        )
    )
