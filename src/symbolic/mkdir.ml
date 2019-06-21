open Format
open Constraints
open Clause
open SymbolicUtility

let name = "mkdir"
         
let interp_mkdir1 cwd path_str =
  under_specifications @@ fun ~root ~root' ->
  let p = Path.from_string path_str in
  match Path.split_last p with
  | None ->
    failure ~error_message:"mkdir: cannot create directory ''" ()
  | Some (_q, (Here|Up)) ->
    failure ~error_message:"mkdir: file exists" () (* CHECK *)
  | Some (q, Down f) ->
    let hintx = last_comp_as_hint ~root q in
    let hinty = Feat.to_string f in [
      success_case
        ~descr:(asprintf "mkdir %a: create directory" Path.pp p)
        begin
          exists2 ?hint1:hintx ?hint2:hintx @@ fun x x' ->
          exists ~hint:hinty @@ fun y ->
          resolve root cwd q x &
          dir x &
          abs x f &
          similar root root' cwd q x x' &
          sim x (Feat.Set.singleton f) x' &
          dir x' &
          feat x' f y &
          dir y &
          fen y Feat.Set.empty
        end;
      error_case
        ~descr:(asprintf "mkdir %a: target already exists" Path.pp p)
        begin
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          dir x &
          nabs x f &
          eq root root'
        end;
      error_case
        ~descr:(asprintf "mkdir %a: parent path is file" Path.pp p)
        begin
          exists ?hint:hintx @@ fun x ->
          resolve root cwd q x &
          ndir x &
          eq root root'
        end;
      error_case
        ~descr:(asprintf "mkdir %a: parent path does not resolve" Path.pp p)
        begin
          noresolve root cwd q &
          eq root root'
        end;
    ]

let interprete ctx : utility =
  match ctx.args with
  | [] -> error ~msg:"mkdir: missing operand" ()
  | [arg] -> interp_mkdir1 ctx.cwd arg
  | _ -> unsupported ~utility:"mkdir" "multiple arguments"
