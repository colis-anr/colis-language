let (||>) f g x = f x |> g

let (>>=) r f =
  match r with
  | Ok x -> f x
  | Error msg -> Error msg

type meta =
  { arguments : string list [@default []] ;
    output : string [@default ""];
    return_code : int [@default 0] }
[@@deriving yojson]

let escape_shell_argument =
  String.split_on_char '\''
  ||> String.concat "'\\''"
  ||> fun s -> "'" ^ s ^ "'"

let in_channel_to_string ic =
  let all = Buffer.create 8 in
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let rec aux () =
    match input ic buf 0 bufsize with
    | 0 -> ()
    | n ->
       Buffer.add_subbytes all buf 0 n;
       aux ()
  in
  aux ();
  Buffer.contents all

let get_meta filename =
  try
    Yojson.Safe.from_file ((Filename.remove_extension filename) ^ ".meta")
    |> meta_of_yojson
    |> function
      | Ok json -> Ok json
      | Error msg -> Error ("meta_of_yojson: " ^ msg)
  with
  | Sys_error msg -> Error ("Sys error: " ^ msg)
  | Yojson.Json_error msg -> Error ("Json error: " ^ msg)

let run_test filename : (unit, string) Result.result =
  get_meta filename
  >>= fun meta ->
  let cmdline =
    [ "colis" ;
      "--" ^ (Filename.extension filename |> function ".cls" -> "colis" | ".sh" -> "shell" | _ -> assert false) ;
      "--run" ;
      filename ]
    @ meta.arguments
    |> List.map escape_shell_argument
    |> String.concat " "
  in
  let stdout = Unix.open_process_in cmdline in
  let output = in_channel_to_string stdout in
  let status = Unix.close_process_in stdout in
  (if output = meta.output then
     Ok ()
   else
     Error
       (let indent =
          String.split_on_char '\n'
          ||> String.concat "\n  > "
        in
        Format.sprintf
          "wrong output\n  expected:\n  > %s\n  got:\n  > %s"
          (indent meta.output)
          (indent output)))
  >>= fun () ->
  (match status with
   | Unix.WEXITED return_code ->
      if return_code = meta.return_code then
        Ok ()
      else
        Error (Format.sprintf
                 "wrong return code (expected %d, got %d)"
                 meta.return_code return_code)
   | _ ->
      Error "execution stopped unexpectedly")

let run_tests () =
  let results =
    (* scan current directory *)
    Sys.readdir Filename.current_dir_name
    |> Array.to_list
    (* take all the .cls and .sh files *)
    |> List.filter (fun name -> Filename.check_suffix name ".cls" || Filename.check_suffix name ".sh")
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

let () =
  run_tests ()
