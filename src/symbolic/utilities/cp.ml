open Format
open Colis_constraints
open Clause
open SymbolicUtility.Symbolic
let name = "cp"

(*
 * Copy the src into a dst which should be an existing directory if todir.
 * Recursive copy if isrec.
 *)
let interp_cp2 ctx ~todir ~isrec src dst : utility =
  let qsrc = Path.from_string src in
  let qdst = Path.from_string dst in
  specification_cases @@
  match Path.split_last qsrc, Path.split_last qdst with
  | (None, _) ->
     [error_case ~descr:"cp: invalid source path ''" noop]
  | (_, None) ->
     [error_case ~descr:"cp: invalid destination path ''" noop]
  | (Some (_, (Here|Up)), _) ->
     (* TODO: "For each source_file, the following steps ...
        source file is dot or dot-dot, cp should do nothing
        with source file and go to any remaining file" *)
    [error_case ~descr:"cp: source path ends in . or .." noop]
  | (_, Some (_, (Here|Up))) -> (* Unreachable or error path *)
    [error_case ~descr:"cp: destination path ends in . or .." noop]
  | (Some (_qs, Down fs), Some (qd, efd)) ->
     let stripdst = Path.strip_trailing_slashes dst in
     let newdst = String.concat "/" [stripdst; (Feat.to_string fs)] in
     let dstpath = Path.from_string newdst in
     let src_no_file_cases =
       [
         error_case
           ~descr:(asprintf "cp: no such file source '%s'" src)
           begin fun root root' ->
             noresolve root ctx.cwd qsrc
             & eq root root'
           end
         ] @ (
         if isrec then []
         else [
           error_case
           ~descr:(asprintf "cp: source '%s' is a directory, option '-R' required" src)
           begin fun root root' ->
             let hintys = last_comp_as_hint ~root qsrc in
             exists ?hint:hintys @@ fun ys ->
             resolve root ctx.cwd qsrc ys & dir ys
             & eq root root'
           end;
           error_case
           ~descr:(asprintf "cp: '%a' is a directory, option '-R' required" Path.pp dstpath)
           begin fun root root' ->
             let hintys = last_comp_as_hint ~root qsrc in
             exists2 ?hint1:hintys ?hint2:None @@ fun ys zd ->
             resolve root ctx.cwd qsrc ys & ndir ys
             & resolve root ctx.cwd dstpath zd & dir zd
             & eq root root'
           end
           ]
         )
     in
     let dst_no_path_cases = [
        error_case
          ~descr:(asprintf "cp: no such file '%a'" Path.pp qd)
          begin fun root root' ->
            noresolve root ctx.cwd qd
            & eq root root'
          end;
        error_case
          ~descr:(asprintf "cp: not a directory '%a'" Path.pp qd)
          begin fun root root' ->
            let hintxd = last_comp_as_hint ~root qd in
            exists ?hint:hintxd @@ fun xd ->
            resolve root ctx.cwd qd xd & ndir xd
            & eq root root'
          end
       ] in
     let dst_no_file_cases =
       match isrec, todir, efd with
       | _, true, _
       | _, false, (Here|Up) -> (* dst no file is an erreor if end in . or .. *)
          [
           error_case
             ~descr:(asprintf "cp: destination directory '%s' does not exist" dst)
            begin fun root root' ->
              noresolve root ctx.cwd qdst & eq root root'
            end
         ]
       | true, false, Down fd -> [
           success_case
             ~descr:(asprintf "cp: source dir '%s' in new dir '%s'" src dst)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintxd = last_comp_as_hint ~root qd in
               exists3 ?hint1:hintys ?hint2:hintxd ?hint3:None @@ fun ys xd xd' ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qd xd & abs xd fd
               & similar root root' ctx.cwd qd xd xd'
               & sim xd (Feat.Set.singleton fd) xd'
               & feat xd' fd ys
             end
         ]
       | false, false, Down fd -> [
           success_case
             ~descr:(asprintf "cp: source file '%s' to new file '%s'" src dst)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               exists ?hint:hintys @@ fun ys ->
               exists2 ?hint1:None ?hint2:None @@ fun xd xd' ->
               resolve root ctx.cwd qsrc ys & ndir ys
               & resolve root ctx.cwd qd xd & dir xd
               & nfen xd (Feat.Set.singleton fd) (* first case *)
               & similar root root' ctx.cwd qd xd xd'
               & sim xd (Feat.Set.singleton fd) xd' & dir xd'
               & feat xd' fd ys
             end;
           success_case
             ~descr:(asprintf "cp file '%s' to new file '%s'" src dst)
             begin fun root root' ->
               let hintys = last_comp_as_hint ~root qsrc in
               let hintyd = last_comp_as_hint ~root qdst in
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               exists2 ?hint1:None ?hint2:None @@ fun xd xd' ->
               resolve root ctx.cwd qsrc ys & ndir ys
               & resolve root ctx.cwd qd xd & dir xd
               & feat xd fd yd & ndir yd (* second case *)
               & similar root root' ctx.cwd qd xd xd'
               & sim xd (Feat.Set.singleton fd) xd' & dir xd'
               & feat xd' fd ys
             end
         ] in
       let src_file_cases = [
           success_case
             ~descr:(asprintf "cp: source file '%s' to directory '%s'" src dst)
           begin fun root root' ->
             let hintys = last_comp_as_hint ~root qsrc in
             let hintyd = last_comp_as_hint ~root qdst in
             exists ?hint:hintys @@ fun ys ->
             exists2 ?hint1:hintyd ?hint2:None @@ fun yd yd' ->
             resolve root ctx.cwd qsrc ys & ndir ys
             & resolve root ctx.cwd qdst yd & dir yd
             & abs yd fs (* first case *)
             & similar root root' ctx.cwd qdst yd yd'
             & sim yd (Feat.Set.singleton fs) yd'
             & dir yd' & feat yd' fs ys
           end;
           success_case
           ~descr:(asprintf "cp: source file '%s' to directory '%s'" src dst)
           begin fun root root' ->
             let hintys = last_comp_as_hint ~root qsrc in
             let hintyd = last_comp_as_hint ~root qdst in
             exists ?hint:hintys @@ fun ys ->
             exists3 ?hint1:hintyd ?hint2:None ?hint3:None @@ fun yd yd' zd ->
             resolve root ctx.cwd qsrc ys & ndir ys
             & resolve root ctx.cwd qdst yd & dir yd
             & feat yd fs zd & ndir zd (* second case *)
             & similar root root' ctx.cwd qdst yd yd'
             & sim yd (Feat.Set.singleton fs) yd'
             & dir yd' & feat yd' fs ys
           end
         ] in
       let src_dir_cases =
         if not(isrec) then [] (* see erreor cases in src_no_file *)
         else [
             success_case
               ~descr:(asprintf "cp: destination dir '%s' does not exist" dst)
               begin fun root root' ->
                 let hintys = last_comp_as_hint ~root qsrc in
                 let hintyd = last_comp_as_hint ~root qdst in
                 exists ?hint:hintys @@ fun ys ->
                 exists2 ?hint1:hintyd ?hint2:None @@ fun yd yd' ->
                 resolve root ctx.cwd qsrc ys & dir ys
                 & resolve root ctx.cwd qdst yd & dir yd
                 & abs yd fs
                 & similar root root' ctx.cwd qdst yd yd'
                 & sim yd (Feat.Set.singleton fs) yd'
                 & dir yd' & feat yd' fs ys
               end;
             success_case
               ~descr:(asprintf "cp: empty directory '%a' exists" Path.pp dstpath)
               begin fun root root' ->
                 let hintys = last_comp_as_hint ~root qsrc in
                 exists2 ?hint1:hintys ?hint2:None @@ fun ys zd ->
                 resolve root ctx.cwd qsrc ys & dir ys
                 & resolve root ctx.cwd dstpath zd & dir zd
                 & fen zd Feat.Set.empty
                 & similar root root' ctx.cwd dstpath zd ys
               end;
             success_case
               ~descr:(asprintf "cp: non empty directory '%a' exists" Path.pp dstpath)
               begin fun root root' ->
                 let hintys = last_comp_as_hint ~root qsrc in
                 exists ?hint:hintys @@ fun ys ->
                 exists2 ?hint1:None ?hint2:None @@ fun zd zd' ->
                 resolve root ctx.cwd qsrc ys & dir ys
                 & resolve root ctx.cwd dstpath zd & dir zd
                 & nfen zd Feat.Set.empty
                 & similar root root' ctx.cwd dstpath zd zd'
                           (* TODO: strange, no copy is done *)
               end
           ]
       in
       List.concat [
           src_no_file_cases; dst_no_path_cases;
           dst_no_file_cases; src_file_cases; src_dir_cases
         ]

let interprete ctx : utility =
  let e = ref None in
  let r = ref false in
  let i = ref false in
  let args_rev = ref [] in
  List.iter
    (function
      | "-H" | "-L" | "-f" | "-p" | "-P" -> () (* Option ignored *)
      | "-i" -> i := true
      | "-r" | "-R" -> r := true
      | arg ->
        if String.length arg > 0 && arg.[0] = '-' && !e = None then
          e := Some arg
        else
          args_rev := arg :: !args_rev)
    ctx.args;
  if !i then
    error ~utility:"cp" "option `-i` forbidden"
  else
    match !e with
    | Some arg -> error ~utility:"cp" ("unknown argument: " ^ arg)
    | None -> (
      let args = List.rev !args_rev in
      match args with
      | [] -> error ~utility:"cp" "missing operand"
      | [_arg] -> error ~utility:"cp" "not enough arguments"
      | [src; dst] -> (* 2 arguments: first, second and third synopsis forms *)
          interp_cp2 ctx ~todir:false ~isrec:!r src dst
      | src::_ -> (* second and third synopsis forms, cp in existing directory *)
        let dir = List.hd !args_rev in
        let srcs = List.rev (List.tl !args_rev) in
        (uand
           (interp_cp2 ctx ~todir:true ~isrec:!r src dir) (* Dest created by the first cp *)
           (multiple_times
              (interp_cp2 ctx ~todir:true ~isrec:!r dir)
              (List.tl srcs)
           )
        )
    )
