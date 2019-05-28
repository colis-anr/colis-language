open Constraints open Clause

let pf = Format.printf

let r = Var.fresh ~hint:"r" ()
let x = Var.fresh ~hint:"x" ()
let y = Var.fresh ~hint:"y" ()

let a = Feat.from_string "a"

let () =
  let output =
    add_to_sat_conj
      (feat r a x & dir x & ndir y & feat r a y)
      true_sat_conj
  in
  match output with
  | [] -> pf "Unsat@."
  | [sat_conj] -> pp_sat_conj Format.std_formatter sat_conj
  | _ -> pf "Sat (> 1)@."
