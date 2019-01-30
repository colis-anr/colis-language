open Misc

let indent s =
  "  > " ^ String.(concat "\n  > " (split_on_char '\n' s))

let run_test filename : (unit, string) Result.result =

  (* Load .meta file *)

  begin
    try
      Ok (Meta.load_from_file
            (Filename.concat !Options.directory ((Filename.remove_extension filename) ^ ".meta")))
    with Sys_error _ -> Error ("Meta file missing for `" ^ filename ^ "`")
  end
  >>= fun meta ->

  (* Build command line *)

  let cmdline =
    [ !Options.utility ]
    @ (if Filename.extension filename = ".cls" then ["--colis"] else [])
    @ [ Filename.concat !Options.directory filename ]
    @ meta.input.arguments
    |> List.map escape_shell_argument
    |> String.concat " "
  in

  (* Execute process *)

  let (stdout, stdin, stderr) = Unix.open_process_full cmdline (Unix.environment ()) in
  output_string stdin meta.input.stdin;
  let stdout_content = in_channel_to_string stdout in
  let stderr_content = in_channel_to_string stderr in
  let status = Unix.close_process_full (stdout, stdin, stderr) in

  (* Check stdout *)

  (if stdout_content = meta.output.stdout then
     Ok ()
   else
     Error
       (Format.sprintf
          "wrong stdout\n  expected:\n%s\n  got:\n%s"
          (indent (String.escaped meta.output.stdout))
          (indent (String.escaped stdout_content))))

  (* Check stderr *)

  >>= fun () ->
  (if stderr_content = meta.output.stderr then
     Ok ()
   else
     Error
       (Format.sprintf
          "wrong stderr\n  expected:\n%s\n  got:\n%s"
          (indent meta.output.stderr)
          (indent stderr_content)))

  (* Check return_code *)

  >>= fun () ->
  (match status with
   | Unix.WEXITED return_code ->
      if return_code = meta.output.return_code then
        Ok ()
      else
        Error (Format.sprintf
                 "wrong return code (expected %d, got %d)"
                 meta.output.return_code return_code)
   | _ ->
      Error "execution stopped unexpectedly")

let run_tests () =
  let results =
    (* scan directory *)
    Sys.readdir !Options.directory
    |> Array.to_list
    (* take all the .cls and .sh files *)
    |> List.filter (fun name -> Filename.check_suffix name ".cls" || Filename.check_suffix name ".sh")
    (* take only .sh files if --sh-only *)
    |> List.filter (fun name -> not (!Options.sh_only) || Filename.check_suffix name ".sh")
    (* run tests on them *)
    |> List.map (fun name -> (name, run_test name))
  in
  List.iter (fun (name, result) ->
      match result with
      | Ok () -> () (*Format.printf "SUCCESS %s@." name*)
      | Error msg -> Format.printf "FAILURE %s: %s@." name msg)
    results;
  let (successes, failures) =
    List.partition (function (_, Ok _) -> true | (_, Error _) -> false) results
  in
  Format.printf "TESTS PASSED: %d / %d@."
    (List.length successes)
    (List.length results);
  exit (if failures = [] then 0 else 1)
