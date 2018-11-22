open Constraints open Clause

let x = Var.fresh ()
let x' = Var.fresh ()
let y = Var.fresh ()

let f = Feat.from_string "f"
let g = Feat.from_string "g"

type sat = Sat | Unsat
type test =
  { formula : Clause.t ;
    expectation : sat }

let tests =
  [
    {
      expectation = Sat ;
      formula =
        exists @@ fun y' ->
        feat x f y & nempty y
        & feat x' f y' & fen y' (Feat.Set.singleton g)
        & sim1 x g x'
    } ;
    {
      expectation = Unsat ;
      formula =
        feat x f y & sim x Feat.Set.empty x' & nfeat x' f y
    }
  ]

let () =
  let res =
    List.fold_left
      (fun res test ->
        match add_to_sat_conj test.formula true_, test.expectation with
        | [], Unsat | _::_, Sat -> res + 1
        | _ -> Format.printf "Failure :-(@."; res)
      0
      tests
  in
  let total = List.length tests in
  Format.eprintf "TESTS PASSED: %d / %d@." res total;
  if res < total then
    exit 1
