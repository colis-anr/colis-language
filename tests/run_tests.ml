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

let run_test filename : (unit, string) Result.result =
  (try Ok (Meta.load_from_file ((Filename.remove_extension filename) ^ ".meta"))
   with Failure msg -> Error ("get_meta: " ^ msg))
  >>= fun meta ->
  let cmdline =
    [ "colis" ;
      "--" ^ (Filename.extension filename |> function ".cls" -> "colis" | ".sh" -> "shell" | _ -> assert false) ;
      "--run" ;
      filename ]
    @ meta.input.arguments
    |> List.map escape_shell_argument
    |> String.concat " "
  in
  let stdout = Unix.open_process_in cmdline in
  let output = in_channel_to_string stdout in
  let status = Unix.close_process_in stdout in
  (if output = meta.output.stdout then
     Ok ()
   else
     Error
       (let indent =
          String.split_on_char '\n'
          ||> String.concat "\n  > "
        in
        Format.sprintf
          "wrong stdout\n  expected:\n  > %s\n  got:\n  > %s"
          (indent meta.output.stdout)
          (indent output)))
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
