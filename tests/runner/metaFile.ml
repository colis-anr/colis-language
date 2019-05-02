open Common

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

let rec promote_null_to_empty_string = function
  | `Null -> `String ""
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `String s -> `String s
  | `A vl -> `A (List.map promote_null_to_empty_string vl)
  | `O svl -> `O (List.map (fun (s, v) -> (s, promote_null_to_empty_string v)) svl)

let load_from_file filename =
  try
    let ichan = open_in filename in
    let yaml =
      in_channel_to_string ichan
      |> yaml_of_string
      |> promote_null_to_empty_string
      |> of_yaml_exn
    in
    close_in ichan;
    yaml
  with
    Not_found -> failwith ("one required key could not be found: "^filename)
