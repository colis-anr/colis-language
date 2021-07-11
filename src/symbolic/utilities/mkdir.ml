open Format
open Colis_constraints
open SymbolicUtility.Mixed
open SymbolicUtility.Mixed.Specifications

let name = "mkdir"

let interp_mkdir1 cwd path_str =
  (* let p = Path.from_string path_str in *)
  let path = Path.strip_trailing_slashes path_str in
  let p = Path.from_string path in
  match Path.split_last p with
  | None ->
    specification_cases [
      error_case ~descr:"mkdir: cannot create directory ''" noop
    ]
  | Some (_q, (Here|Up)) ->
    specification_cases [
      error_case ~descr:"mkdir: file exists" noop (* CHECK *)
    ]
  | Some (q, Down f) ->
    specification_cases [
      success_case
        ~descr:(asprintf "mkdir %a: create directory" Path.pp p)
        (case_spec
           ~concrete:begin fun fs ->
             let path = List.map Feat.to_string (cwd @ [f]) in
             try Some (FilesystemSpec.add_dir path fs)
             with Invalid_argument _ -> None
           end
           ~transducers:()
           ~constraints:begin fun root root' ->
             exists3 @@ fun x x' y ->
             resolve root cwd q x &
             dir x &
             abs x f &
             similar root root' cwd q x x' &
             sim x (Feat.Set.singleton f) x' &
             dir x' &
             feat x' f y &
             dir y &
             fen y Feat.Set.empty
           end
           ());
      error_case
        ~descr:(asprintf "mkdir %a: target already exists" Path.pp p)
        (case_spec
          ~constraints:begin fun root root' ->
             exists @@ fun x ->
             resolve root cwd q x &
             dir x &
             nabs x f &
             eq root root'
           end ());
      error_case
        ~descr:(asprintf "mkdir %a: parent path is file or does not resolve" Path.pp p)
        (case_spec
           ~constraints:begin fun root root' ->
             exists @@ fun x ->
             maybe_resolve root cwd q x
             & ndir x
             & eq root root'
           end ());
    ]

let interprete parents ctx args : utility =
  if parents then Interpreter.incomplete ~utility:name "option -p" else
  Combinators.multiple_times (interp_mkdir1 ctx.Interpreter.cwd) args

let interprete ctx : utility =
  let parents = Cmdliner.Arg.(value & flag & info ["p"; "parents"]) in
  Combinators.cmdliner_eval_utility
    ~utility:name
    Cmdliner.Term.(const interprete $ parents)
    ctx
