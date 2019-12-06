(** Concrete interpretation of selected shell builtins and some UNIX commands.

    For the shell builtins see subsections of http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap04.html#tag_20
  *)

open Colis_internals
open Semantics__Buffers
open Interpreter__Semantics

module IdMap = Env.IdMap

type env = string IdMap.t

let unsupported ~utility msg = fun sta ->
  if !Options.fail_on_unknown_utilities then
    raise (Errors.Unsupported (utility, msg))
  else
    let str = utility ^ ": " ^ msg in
    let stdout = Stdout.(sta.stdout |> output str |> newline) in
    {sta with stdout}, false

let unknown_utility utility = fun sta ->
  if !Options.fail_on_unknown_utilities then
    raise (Errors.Unsupported (utility, "unknown utility"))
  else
    let str = utility ^ ": command not found" in
    let stdout = Stdout.(sta.stdout |> output str |> newline) in
    {sta with stdout}, false

let test (sta : state) : string list -> (state * bool) = function
  | [sa; "="; sb] ->
     (sta, sa = sb)
  | [sa; "!="; sb] ->
     (sta, sa <> sb)
  | _ ->
    unsupported ~utility:"test" "arguments different from . = . and . != ." sta

let dpkg_compare_versions args =
  Sys.command ("dpkg --compare-versions " ^ String.concat " " args) = 0

let dpkg_validate_thing subcmd arg =
  Sys.command ("dpkg " ^ subcmd ^ " " ^arg) = 0

let interp_utility (cwd, var_env, args) id sta =
  match id with
  | "echo" ->
     let stdout =
       match args with
       | "-n" :: args ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str)
       | _ ->
          let str = String.concat " " args in
          Stdout.(sta.stdout |> output str |> newline)
     in
     {sta with stdout}, true
  | ":" | "true" ->
     sta, true
  | "false" ->
     sta, false
  | "test" -> test sta args
  | "env" ->
    begin match args with
    | [] ->
      let format_line (var, value) =
        Format.sprintf "%s=%s" var value
      in
      let print_line sta line =
        {sta with stdout=Stdout.(sta.stdout |> output line |> newline)}
      in
      IdMap.bindings var_env |>
      List.map format_line |>
      List.fold_left print_line sta, true
    | _arg :: _ ->
      unsupported ~utility:"env" "no arguments supported" sta
    end
  | "grep" -> (* Just for testing stdin/stdout handling *)
     begin match args with
     | [word] ->
        let stdout, result =
          let re = Str.regexp_string word in
          let f (stdout, res) line =
            try
              ignore (Str.search_forward re line 0);
              Stdout.(stdout |> output line |> newline), true
            with Not_found ->
              stdout, res
          in
          List.fold_left f (sta.stdout, false) sta.stdin
        in
        let sta' = {sta with stdout; stdin=Stdin.empty} in
        sta', result
     | [] ->
       unsupported ~utility:"grep" "missing argument" sta
     | _arg :: _ ->
       unsupported ~utility:"grep" "two or more arguments" sta
     end
  | "dpkg" ->
     begin match args with
     | (("--validate-pkgname" | "--validate-trigname" |
         "--validate-archname" | "--validate-version") as subcmd)::args->
        if List.length args = 1
        then sta, dpkg_validate_thing subcmd (List.hd args)
        else unsupported ~utility:"dpkg"
               "--validate_thing needs excactly 1 argument" sta
     | "--compare-versions"::args ->
      if List.length args = 3
      then sta, dpkg_compare_versions args
      else unsupported ~utility:"dpkg"
             "--compare-versions needs excatly 3 arguments" sta
     | _ -> unsupported ~utility:"dpkg" "unsupported arguments" sta
     end
  | _ -> unknown_utility id sta

let absolute_or_concat_relative (p: string list) (s: string) : string list =
  if String.equal s "" then
    p
  else
    let p' =
      String.split_on_char '/' s |>
      List.filter (fun s' -> not (String.equal s' ""))
    in
    if s.[0] = '/' then
      p'
    else
      p @ p'
