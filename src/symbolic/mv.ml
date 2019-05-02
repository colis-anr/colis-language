open Format
open Constraints
open Clause
open SymbolicUtility

let name = "mv"

let interp_mv ctx src dst : utility =
  under_specifications @@ fun ~root ~root' ->
    let qsrc = Path.from_string src in
    let qdst = Path.from_string dst in
    (* FIXME: case when qdst end in / but denotes a non existing file *)
    let _norm_qsrc = Path.normalize ~cwd:ctx.cwd qsrc in
    let _norm_qdst = Path.normalize ~cwd:ctx.cwd qdst in
    (* FIXME: Put in lib/constraints/common/Path.mli
     *        test that norm_qsrc is not a prefix or equal to norm_qdst *)
    (*        this test should be done on normalized paths in an utility *)
    (*        which return true if the source is a parent on the destination *)
    match Path.split_last qsrc, Path.split_last qdst with
    | (None, _) ->
       failure ~error_message:"mv: invalid source path ''" ()
    | (_, None) ->
       failure ~error_message:"mv: invalid destination path ''" ()
    | (Some (_, (Here|Up)), _) | (_, Some(_, (Here|Up))) ->
       failure ~error_message:"mv: not a normalized path" () (* FIXME: check! *)
    | (Some (qs, Down fs), Some (qd, Down fd)) ->
       let hintxs = last_comp_as_hint ~root qs in
       let hintys = last_comp_as_hint ~root qsrc in
       let hintxd = last_comp_as_hint ~root qd in
       let hintyd = last_comp_as_hint ~root qdst in [
           (* FIXME: the following two success cases shall be grouped in one *)
           success_case
             ~descr:(asprintf "mv: rename file '%s' to file '%s'" src dst)
             begin
               exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
               exists3 ?hint1:hintxd ?hint2:hintyd ?hint3:hintxd @@ fun xd yd xd' ->
               exists3 ?hint1:None ?hint2:hintxs ?hint3:hintxd @@ fun ri xsi xdi ->
    (* FIXME: if qdst ends with / then rename
     *        else match below shall be on norm_qsrc and norm_qdst *)
               resolve root ctx.cwd qsrc ys & ndir ys
               & resolve root ctx.cwd qd xd
               & abs xd fd (* FIXME: first case *)
               & similar root ri ctx.cwd qs xs xsi
               & sim xs (Feat.Set.singleton fs) xsi
               & abs xsi fs
               & similar ri root' ctx.cwd qd xdi xd'
               & sim xdi (Feat.Set.singleton fd) xd'
               & feat xd' fd ys
             end;
          success_case
            ~descr:(asprintf "mv: rename file '%s' to file '%s'" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintxd ?hint2:hintyd ?hint3:None @@ fun xd yd xd' ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi xdi ->
              resolve root ctx.cwd qsrc ys & ndir ys
              & resolve root ctx.cwd qd xd
              & feat xd fd yd & ndir yd (* FIXME: second case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qd xdi xd'
              & sim xdi (Feat.Set.singleton fd) xd'
              & feat xd' fd ys
            end;
          (* FIXME: the following two success cases shall be grouped in one *)
          success_case
            ~descr:(asprintf "mv: file '%s' to directory '%s'" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintxd ?hint2:hintyd ?hint3:None @@ fun xd yd yd' ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi ydi ->
              resolve root ctx.cwd qsrc ys & ndir ys
              & resolve root ctx.cwd qdst yd
              & abs yd fs (* FIXME: first case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qdst ydi yd'
              & sim ydi (Feat.Set.singleton fs) yd'
              & feat yd' fs ys
            end;
          success_case
            ~descr:(asprintf "mv: file '%s' to directory '%s'" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintxd ?hint2:hintyd ?hint3:None @@ fun xd yd yd' ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi ydi ->
              exists  ?hint:None @@ fun zd -> (* FIXME: second case *)
              resolve root ctx.cwd qsrc ys & ndir ys
              & resolve root ctx.cwd qdst yd
              & feat yd fs zd & ndir zd (* FIXME: second case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qdst ydi yd'
              & sim ydi (Feat.Set.singleton fs) yd'
              & feat yd' fs ys
            end;
          success_case
            ~descr:(asprintf "mv: source '%s' is a directory, destination '%s' is not" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists2 ?hint1:hintxd ?hint2:None @@ fun xd xd' ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi xdi ->
              resolve root ctx.cwd qsrc ys & dir ys
              & resolve root ctx.cwd qd xd & abs xd fd
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi & abs xsi fs
              & similar ri root' ctx.cwd qd xdi xd'
              & sim xdi (Feat.Set.singleton fd) xd'
              & feat xd' fs ys
            end;
          (* FIXME: the following two success cases shall be grouped in one *)
          success_case
            ~descr:(asprintf "mv: source	'%s' and destination '%s' are directories" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintyd ?hint2:None ?hint3:None @@ fun yd yd' zd ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi ydi ->
              resolve root ctx.cwd qsrc ys & dir ys
              & resolve root ctx.cwd qdst yd
              & abs yd fs (* FIXME: first case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qdst ydi yd'
              & sim ydi (Feat.Set.singleton fs) yd'
              & feat yd' fs ys
            end;
          success_case
            ~descr:(asprintf "mv: source '%s' and destination '%s' are directories" src dst)
            begin
              exists2 ?hint1:hintxs ?hint2:hintys @@ fun xs ys ->
              exists3 ?hint1:hintyd ?hint2:None ?hint3:None @@ fun yd yd' zd ->
              exists3 ?hint1:None ?hint2:None ?hint3:None @@ fun ri xsi ydi ->
              resolve root ctx.cwd qsrc ys & dir ys
              & resolve root ctx.cwd qdst yd
              & feat yd fs zd & fen zd Feat.Set.empty (* FIXME: second case *)
              & similar root ri ctx.cwd qs xs xsi
              & sim xs (Feat.Set.singleton fs) xsi
              & abs xsi fs
              & similar ri root' ctx.cwd qdst ydi yd'
              & sim ydi (Feat.Set.singleton fs) yd'
              & feat yd' fs ys
      	    end;
          error_case
            ~descr:(asprintf "mv: source path '%a' does not resolve" Path.pp qsrc)
            begin
              noresolve root ctx.cwd qsrc &
              eq root root'
            end;
          error_case
            ~descr:(asprintf "mv: destination parent path '%a' does not resolve" Path.pp qd)
            begin
              noresolve root ctx.cwd qd &
              eq root root'
            end;
          error_case
            ~descr:(asprintf "mv: destination '%a' not a directory" Path.pp qd)
            begin
              exists ?hint:hintxd @@ fun xd ->
              resolve root ctx.cwd qd xd &
              ndir xd &
              eq root root'
            end;
          error_case
            ~descr:(asprintf "mv: destination file '%a/%a' not empty" Path.pp qdst Feat.pp fs)
            begin
              exists ?hint:None @@ fun zd ->
              resolve root ctx.cwd (Path.from_string
                      (String.concat "" [dst; "/"; (Feat.to_string fs)])) zd &
              fen zd Feat.Set.empty &
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
    | "-f" :: args -> interprete {ctx with args}
    | "-i" :: args ->
      under_specifications @@ fun ~root ~root' ->
        [
          error_case
            ~descr:(asprintf "mv: option '-i' forbidden")
            begin
              eq root root'
            end
        ]
    | [arg] ->
      under_specifications @@ fun ~root ~root' ->
        [
          error_case
            ~descr:(asprintf "mv: not enough arguments")
            begin
              eq root root'
            end
        ]
    | [src; dst] -> interp_mv ctx src dst
    | _ ->
      under_specifications @@ fun ~root ~root' ->
        [
          error_case
            ~descr:(asprintf "mv: unknown option or arguments forbidden")
            begin
              eq root root'
            end
        ]