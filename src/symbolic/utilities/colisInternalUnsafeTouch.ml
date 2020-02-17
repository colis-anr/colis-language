open Format
open Colis_constraints
open Clause
open SymbolicUtility

let name = "colis_internal_unsafe_touch"

let interp1 path_str =
  let p = Path.from_string path_str in
  let q = Path.check_normal p in
  under_specifications [
    success_case
      ~descr:(asprintf "colis_internal_unsafe_touch: %a" Path.pp p)
      begin fun root root' ->
        (* FIXME: one could do better and drop the last maybe on the left. *)
        let rec aux x x' = function
          | [] ->
            reg x'
          | f :: q ->
            exists2 @@ fun y y' ->
            sim x (Feat.Set.singleton f) x'
            & maybe x f y & feat x' f y'
            & aux y y' q
        in
        aux root root' q
      end
  ]

let interprete _ctx args : utility =
  multiple_times interp1 args

let interprete ctx : utility =
  cmdliner_eval_utility ~utility:name Cmdliner.Term.(const interprete) ctx
