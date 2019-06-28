open Format
open Constraints
open Clause
open SymbolicUtility

let name = "mv"

(*  Specification of 'rename()' system call
 *    used by 'mv'
 *)
let interp_rename ctx src dstpath : utility =
  under_specifications @@ fun ~root ~root' ->
    let qsrc = Path.from_string src in
    let qdst = Path.from_string dstpath in
    match Path.split_last qsrc, Path.split_last qdst with
    | (None, _) ->
       failure ~error_message:"mv: invalid source path ''" ()
    | (_, None) ->
       failure ~error_message:"mv: invalid destination path ''" ()
    | (Some (_, (Here|Up)), _) | (_, Some(_, (Here|Up))) ->
       failure ~error_message:"mv: paths end in . or .." ()
    | (Some (qs, Down fs), Some (qd, Down fd)) ->
       let hintxs = last_comp_as_hint ~root qs in
       let hintys = last_comp_as_hint ~root qsrc in
       let hintxd = last_comp_as_hint ~root qd in
       let hintyd = last_comp_as_hint ~root qdst in
       let unconditional_cases = [
           error_case
             ~descr:(asprintf "mv/rename: old '%s' does not resolve" src)
             begin
               noresolve root ctx.cwd qsrc
               & eq root root'
             end;
           error_case
             ~descr:(asprintf "mv/rename: no path to new '%s'" dstpath)
             begin
               noresolve root ctx.cwd qd
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: same file")
             begin
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys
               & resolve root ctx.cwd qdst yd
               & eq ys yd
               & eq root root'
             end;
           error_case
             ~descr:(asprintf "mv/rename: old '%s' is a directory, new '%s' is not"
                       src dstpath)
             begin
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qdst yd & ndir yd
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: old '%s' is directory, new '%s' is an empty directory"
                       src dstpath)
             begin
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
             begin
               exists2 ?hint1:hintys ?hint2:hintyd @@ fun ys yd ->
               resolve root ctx.cwd qsrc ys & dir ys
               & resolve root ctx.cwd qdst yd & dir yd
               & neq ys yd & nfen yd Feat.Set.empty
               & eq root root'
             end;
           success_case
             ~descr:(asprintf "mv/rename: old '%s' is file, new '%s' is absent"
                       src dstpath)
             begin
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
             begin
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
            begin
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
             begin
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
               begin
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
               begin
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


let interp_mv2dir ctx src dst : utility =
  (* Assume: dst resolves to an existing directory *)
  let qsrc = Path.from_string src in
  match Path.split_last qsrc with
  | None ->
     error ~msg:"mv: invalid source path ''" ()
  | Some (_, (Here|Up)) ->
     error ~msg:"mv: source path ends in . or .." ()
  | Some (_, Down fs) ->
     let stripdst = Path.strip_trailing_slashes dst in
     let dstpath = String.concat "/" [stripdst; (Feat.to_string fs)] in
     interp_rename ctx src dstpath


let interp_mv2dir_fold _ctx _sources _dst = (* TODO *)
  under_specifications @@
    fun ~root ~root' ->
    [
      success_case
        ~descr:(asprintf "mv: several sources")
        begin
          eq root root'
        end
    ]


let rec interprete ctx : utility =
  match ctx.args with
  | [] ->
     under_specifications @@ fun ~root ~root' ->
         [
           error_case
             ~descr:(asprintf "mv: missing operands")
             begin
               eq root root'
             end
         ]
  | "-f" :: args -> interprete {ctx with args} (* By default option *)
  | "-i" :: _ ->
     under_specifications @@ fun ~root ~root' ->
         [
           error_case
             ~descr:(asprintf "mv: option '-i' forbidden")
             begin
               eq root root'
             end
         ]
  | [_arg] ->
     under_specifications @@ fun ~root ~root' ->
         [
           error_case
             ~descr:(asprintf "mv: not enough arguments")
             begin
               eq root root'
             end
         ]
  | [src; dst] ->
     (if_then_else
        (call "test" ctx ["-d"; dst])
        (interp_mv2dir ctx src dst)
        (interp_rename ctx src dst)
     )
  | _ ->
     (* More than two arguments, then mv in directory *)
     match (List.rev ctx.args) with
     | [] -> (* Not reachable *)
        under_specifications @@ fun ~root ~root' ->
           [
             error_case
               ~descr:(asprintf "mv: missing operands")
               begin
                 eq root root'
               end
           ]
     | dir :: srcs ->
        (if_then_else
           (call "test" ctx ["-d"; dir])
           (interp_mv2dir_fold ctx (List.rev srcs) dir)
           (under_specifications @@ fun ~root ~root' ->
                [
                  error_case
                    ~descr:(asprintf "mv: last argument not a directory")
                    begin
                      eq root root'
                    end
                ]
           )
        )

