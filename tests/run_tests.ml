let (||>) f g x = f x |> g

let (>>=) r f =
  match r with
  | Ok x -> f x
  | Error msg -> Error msg

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

module Meta =
  struct
    let yaml_of_string = Yaml.of_string_exn
    open Protocol_conv_yaml

    type input =
      { arguments : string list ;
        stdin : string }
        [@@deriving protocol ~driver:(module Yaml)]

    type output =
      { stdout : string ;
        stderr : string ;
        return_code : int }
        [@@deriving protocol ~driver:(module Yaml)]

    type t =
      { input : input ;
        output : output }
        [@@deriving protocol ~driver:(module Yaml)]

    let rec promote_strings_to_ints = function
      | `Null -> `Null
      | `Bool b -> `Bool b
      | `Float f -> `Float f
      | `String s ->
         (try `Float (float_of_string s)
          with _ -> `String s)
      | `A vl -> `A (List.map promote_strings_to_ints vl)
      | `O svl -> `O (List.map (fun (s, v) -> (s, promote_strings_to_ints v)) svl)

    let load_from_file filename =
      try
        let ichan = open_in filename in
        let yaml =
          in_channel_to_string ichan
          |> yaml_of_string
          |> promote_strings_to_ints
          |> of_yaml
        in
        close_in ichan;
        yaml
      with
        Not_found -> failwith "one required key could not be found"
  end

let indent s =
  "  > " ^ String.(concat "\n  > " (split_on_char '\n' s))

let run_test filename : (unit, string) Result.result =

  (* Load .meta file *)

  let meta =
    Meta.load_from_file
      ((Filename.remove_extension filename) ^ ".meta")
  in

  (* Build command line *)

  let cmdline =
    [ "colis" ;
      "--" ^ (Filename.extension filename |> function ".cls" -> "colis" | ".sh" -> "shell" | _ -> assert false) ;
      "--run" ;
      filename ]
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
          (indent meta.output.stdout)
          (indent stdout_content)))

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
