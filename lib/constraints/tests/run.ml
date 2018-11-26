open Constraints open Clause

let x = Var.fresh ~hint:"x" ()
let x' = Var.fresh ~hint:"x" ()
let y = Var.fresh ~hint:"y" ()

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
        exists ~hint:"y" @@ fun y' ->
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
    (0, 0)
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
