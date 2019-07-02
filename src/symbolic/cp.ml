open Format
open Constraints
open Clause
open SymbolicUtility

let name = "mv"

(* Copy file src to directory dst.
 * Recursive copy if isrec.
 * assumes: dst resolves to an existing directory
 *)
let interp_cp2dir ctx isrec dst src : utility =
  under_specifications @@ fun ~root ~root' ->
  let qsrc = Path.from_string src in
  let qdst = Path.from_string dst in
  match Path.split_last qsrc, Path.split_last qdst with
  | (None, _) ->
     failure ~error_message:"cp: invalid source path ''" ()
  | (_, None) ->
     failure ~error_message:"cp: invalid destination path ''" ()
  | (Some (_, (Here|Up)), _) ->
     failure ~error_message:"cp: source path ends in . or .." ()
  | (Some (_qs, Down fs), _) -> (* Some (qd, Down fd)) -> *)
     let stripdst = Path.strip_trailing_slashes dst in
     let newdst = String.concat "/" [stripdst; (Feat.to_string fs)] in
     let dstpath = Path.from_string newdst in
     let src_no_file_cases = [
         error_case
           ~descr:(asprintf "cp: source '%s' does not resolve" src)
           begin
             noresolve root ctx.cwd qsrc
             & eq root root'
           end
       ]
     in
     let hintys = last_comp_as_hint ~root qsrc in
     let hintyd = last_comp_as_hint ~root qdst in
     let src_file_cases = [
         success_case
           ~descr:(asprintf "cp file '%s' to directory '%s'" src dst)
           begin
             exists ?hint:hintys @@ fun ys ->
             exists2 ?hint1:hintyd ?hint2:None @@ fun yd yd' ->
             resolve root ctx.cwd qsrc ys & ndir ys
             & resolve root ctx.cwd qdst yd & dir yd (* Not needed *)
             & abs yd fs (* first case *)
             & similar root root' ctx.cwd qdst yd yd'
             & sim yd (Feat.Set.singleton fs) yd'
             & dir yd' & feat yd' fs ys
           end;
         success_case
           ~descr:(asprintf "cp file '%s' to directory '%s'" src dst)
           begin
             exists ?hint:hintys @@ fun ys ->
             exists3 ?hint1:hintyd ?hint2:None ?hint3:None @@ fun yd yd' zd ->
             resolve root ctx.cwd qsrc ys & ndir ys
             & resolve root ctx.cwd qdst yd & dir yd (* Not needed *)
             & feat yd fs zd & ndir zd (* second case *)
             & similar root root' ctx.cwd qdst yd yd'
             & sim yd (Feat.Set.singleton fs) yd'
             & dir yd' & feat yd' fs ys
           end
       ]
     in
     let src_subdir_exists_cases =
       if isrec then (* with option '-R' *)
         [
           success_case
             ~descr:(asprintf "cp non empty directory '%s' exists in '%s'" (Feat.to_string fs) dst)
             begin
               exists ?hint:hintys @@ fun ys ->
               exists2 ?hint1:None ?hint2:None @@ fun zd zd' ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd dstpath zd & dir zd
               & nfen zd Feat.Set.empty
               & similar root root' ctx.cwd dstpath zd zd'
                         (* TODO: strange, no copy is done *)
             end
         ]
       else
         [
           error_case
             ~descr:(asprintf "cp source '%s' is directory" src)
             begin
               exists ?hint:hintys @@ fun ys ->
               resolve root ctx.cwd qsrc ys & dir ys
               & eq root root'
             end;
           error_case
             ~descr:(asprintf "cp source file '%s' to directory '%s' required" src newdst)
             begin
               exists ?hint:None @@ fun zd ->
               resolve root ctx.cwd dstpath zd & dir zd
               & nfen zd Feat.Set.empty
               & eq root root'
             end
         ] in
     List.concat [ src_no_file_cases; src_file_cases; src_subdir_exists_cases ]

(*
 * Copy the src into a dst which is not an existing directory.
 * Recursive copy if isrec.
 * assumes: dst is not an existing directory
 *)
let interp_cp2 ctx isrec src dst : utility =
  under_specifications @@ fun ~root ~root' ->
  let qsrc = Path.from_string src in
  let qdst = Path.from_string dst in
  match Path.split_last qsrc, Path.split_last qdst with
  | (None, _) ->
     failure ~error_message:"cp: invalid source path ''" ()
  | (_, None) ->
     failure ~error_message:"cp: invalid destination path ''" ()
  | (Some (_, (Here|Up)), _) ->
     failure ~error_message:"cp: source path ends in . or .." ()
  | (_, Some (_, (Here|Up))) -> (* Unreachable or error path *)
     failure ~error_message:"cp: destination path ends in . or .." ()
  | (Some (_qs, Down _fs), Some (qd, Down fd)) ->
     (* let stripdst = Path.strip_trailing_slashes dst in
     let newdst = String.concat "/" [stripdst; (Feat.to_string fs)] in
     let dstpath = Path.from_string newdst in *)
     let hintys = last_comp_as_hint ~root qsrc in
     let hintxd = last_comp_as_hint ~root qd in
     let hintyd = last_comp_as_hint ~root qdst in
     let dest_no_file_cases =
       if isrec then
         [
           success_case
             ~descr:(asprintf "cp source dir '%s' in new dir '%s'" src dst)
             begin
               exists3 ?hint1:hintys ?hint2:hintxd ?hint3:None @@ fun ys xd xd' ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qd xd & abs xd fd
               & similar root root' ctx.cwd qd xd xd'
               & sim xd (Feat.Set.singleton fd) xd'
               & feat xd' fd ys
             end
         ]
       else
         [
           success_case
             ~descr:(asprintf "cp file '%s' to new file '%s'" src dst)
             begin
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
             begin
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               exists2 ?hint1:None ?hint2:None @@ fun xd xd' ->
               resolve root ctx.cwd qsrc ys & ndir ys
               & resolve root ctx.cwd qd xd & dir xd
               & feat xd fd yd & ndir yd (* second case *)
               & similar root root' ctx.cwd qd xd xd'
               & sim xd (Feat.Set.singleton fd) xd' & dir xd'
               & feat xd' fd ys
             end
         ]
       in
       let src_no_file_cases = [
           error_case
             ~descr:(asprintf "cp: source '%s' does not resolve" src)
             begin
               noresolve root ctx.cwd qsrc
               & eq root root'
             end
         ]
       in
       let src_is_dir_cases =
         if isrec then []
         else [
           error_case
           ~descr:(asprintf "cp: source '%s' is a directory, option '-R' required" src)
           begin
             exists ?hint:hintys @@ fun ys ->
             resolve root ctx.cwd qsrc ys & dir ys
             & eq root root'
           end
         ]
       in
       List.concat [ dest_no_file_cases; src_no_file_cases; src_is_dir_cases ]

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
    error ~msg:"cp: option `-i` forbidden" ()
  else
    match !e with
    | Some arg -> unsupported ~utility:"cp" ("unknown argument: " ^ arg)
    | None -> (
      let args = List.rev !args_rev in
      match args with
      | [] -> error ~msg:"cp: missing operand" ()
      | [_arg] -> error ~msg:"cp: not enough arguments" ()
      | [src; dst] -> (* 2 arguments: first, second and third synopsis forms *)
          interp_cp2 ctx !r src dst
      | src::_ -> (* second and third synopsis forms, cp in existing directory *)
        let dir = List.hd !args_rev in
        let srcs = List.rev (List.tl !args_rev) in
        (if_then_else
           (call "test" ctx ["-d"; dir])
           (multiple_times (interp_cp2dir ctx !r dir) srcs)
           (uand
              (interp_cp2 ctx !r src dir) (* Dest created by the first cp *)
              (multiple_times (interp_cp2dir ctx !r dir) (List.tl srcs)))
        )
    )
