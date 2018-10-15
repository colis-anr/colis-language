open Misc

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
    Not_found -> failwith ("one required key could not be found: "^filename)
