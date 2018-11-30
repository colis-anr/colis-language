open Constraints open Clause

let x = Var.fresh ~hint:"x" ()
let x' = Var.fresh ~hint:"x" ()
let y = Var.fresh ~hint:"y" ()
let z = Var.fresh ~hint:"z" ()

let f = Feat.from_string "f"
let g = Feat.from_string "g"

type sat = Sat | Unsat
type test =
  { formula : Clause.t ;
    expectation : sat }

let tests =
  [
    { expectation = Sat ;
      formula = exists @@ fun y' -> feat x f y & nempty y & feat x' f y' & fen y' (Feat.Set.singleton g) & sim1 x g x' } ;
    { expectation = Unsat ;
      formula = feat x f y & sim x Feat.Set.empty x' & nfeat x' f y } ;
    { expectation = Unsat ;
      formula = nsim x Feat.Set.empty y & ndir x & ndir y } ;
    { expectation = Sat ;
      formula = sim1 x g y & fen x (Feat.Set.singleton f) & nfen y (Feat.Set.singleton f) } ;
    { expectation = Unsat ;
      formula = sim x Feat.Set.empty y & feat x f y } ;
    { expectation = Sat ;
      formula = fen x (Feat.Set.of_list [f; g]) & nsim x Feat.Set.empty y & abs y f & abs y g } ;
    { expectation = Unsat ;
      formula = sim1 x f y & sim1 y g z & nsim x Feat.Set.empty z & empty z & abs x f & abs x g } ;
    { expectation = Unsat ;
      formula = feat x f y & abs y f & exists @@ fun y' -> feat x f y' & feat y' f z } ;
    { expectation = Sat ;
      formula = sim1 x f y & nsim x Feat.Set.empty z & abs x f & abs z f & fen y (Feat.Set.singleton g) } ;
  ]

let src = Logs.Src.create "colis-language.constraints.test" ~doc:"Logging from the constraints' test engine"
module Log = (val Logs.src_log src : Logs.LOG)

let run_tests () =
  List.fold_left
    (fun (res, i) test ->
      Log.info (fun m -> m "Running test %d (%s expected)."
                           i
                           (match test.expectation with Sat -> "Sat" | _ -> "Unsat"));
      (
        match add_to_sat_conj test.formula true_, test.expectation with
        | [], Unsat | _::_, Sat ->
           (
             Log.info (fun m -> m "Test %d succeeded" i);
             res + 1
           )
        | _ ->
           (
             Log.warn (fun m -> m "Test %d failed" i);
             res
           )
      ),
      (i + 1))
    (0, 1)
    tests
  |> fst

let () =
  Logs.(set_reporter (Logs_fmt.reporter ()));
  Logs.(set_level (Some Debug));
  let res = run_tests () in
  let total = List.length tests in
  Format.eprintf "TESTS PASSED: %d / %d@." res total;
  if res < total then
    exit 1
