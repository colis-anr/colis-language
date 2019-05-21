open Format
open Constraints

module SMap = Map.Make(String)

type t = node SMap.t
and node = Dir of t | File

let empty : t = SMap.empty

let rec add sofar typ path t =
  match path with
  | [] -> t
  | [name] ->
    if name = "." || name = ".." then
      ksprintf invalid_arg "FilesystemSpec.add: %s/%s" sofar name;
    begin match typ, SMap.find_opt name t with
      | _, None ->
        let n = match typ with `File -> File | `Dir -> Dir SMap.empty in
        SMap.add name n t
      | `Dir, Some (Dir _) -> t
      | `File, Some File -> t
      | `Dir, Some File -> ksprintf invalid_arg "FilesystemSpec.add: directory %s/%s is already a file" sofar name
      | `File, Some (Dir _) -> ksprintf invalid_arg "FilesystemSpec.add: file %s/%s is already a directory" sofar name
    end
  | name :: path' ->
    if name = "." || name = ".." then
      ksprintf invalid_arg "FilesystemSpec.add: %s/%s" sofar name;
    let t' =
      match SMap.find_opt name t with
      | None -> SMap.empty
      | Some (Dir t) -> t
      | Some File -> ksprintf invalid_arg "FilesystemSpec.add: directory %s/%s is already a file" sofar name
    in
    let t' = add (sofar^"/"^name) typ path' t' in
    SMap.add name (Dir t') t

let add_file = add "" `File
let add_dir = add "" `Dir

let add_channel cin t =
  let t = ref t in
  try
    while true do
      let line = input_line cin in
      match String.split_on_char '/' line with
      | [] -> assert false
      | "" :: path -> (* Line starts with '/' *)
        t := begin match List.rev path with
          | [""] -> !t (* Empty line*)
          | "" :: path' -> (* Line ends with / *)
            add_dir (List.rev path') !t
          | path' -> (* Line does not end with '/' *)
            add_file (List.rev path') !t
        end
      | _ -> (* Line does not start with '/' *)
        ksprintf invalid_arg "FilesystemSpec.add_channel: line does not start with '/': %s" line
    done;
    assert false
  with End_of_file -> !t

let rec compile root t =
  SMap.fold (fun name node -> Clause.and_ @@ compile_node root name node) t Clause.true_

and compile_node x name node =
  let f = Feat.from_string name in
  let open Clause in
  exists ~hint:name @@ fun y ->
  feat x f y &
  match node with
  | File -> ndir y
  | Dir t -> dir y & compile y t

let rec pp fmt t =
  SMap.bindings t |>
  List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2) |>
  List.iteri @@ fun ix (name, node) ->
  fprintf fmt "- @[%s%a@]" name pp_node node;
  if ix+1 <> SMap.cardinal t then
    pp_print_newline fmt ()
and pp_node fmt = function
  | File -> fprintf fmt ""
  | Dir t ->
    pp_print_char fmt '/';
    if not (SMap.is_empty t) then
      fprintf fmt "@\n%a" pp t
