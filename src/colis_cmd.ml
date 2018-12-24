open Format

type source = Colis | Shell
type action = Run | RunSymbolic | PrintColis | PrintShell

let (||>) f g x = f x |> g

let get_action, set_action =
  let action = ref None in
  (fun () ->
    match !action with
    | None -> Run
    | Some action -> action),
  (fun new_action () ->
    match !action with
    | None -> action := Some new_action
    | Some _ -> raise (Arg.Bad "only one action among --run, --run-symbolic, --print-colis and --print-shell can be specified"))

let realworld = ref false

let get_source, set_source =
  let source = ref None in
  (fun () ->
    match !source with
    | None -> Shell
    | Some source -> source),
  (fun new_source () ->
    match !source with
    | None -> source := Some new_source
    | Some _ -> raise (Arg.Bad "only one source among --colis and --shell can be specified"))

let get_file, get_arguments, set_file_or_argument =
  let file = ref None in
  let args = ref [] in
  (fun () ->
    match !file with
    | None -> raise (Arg.Bad "you must provide an input file")
    | Some file -> file),
  (fun () -> List.rev !args),
  (fun new_file_or_arg ->
    match !file with
    | None -> file := Some new_file_or_arg
    | Some _ -> args := new_file_or_arg :: !args)

let get_symbolic_fs, set_symbolic_fs =
  let fs_spec = ref Colis.Symbolic.FilesystemSpec.empty in
  (fun () -> !fs_spec),
  (fun str ->
     let open Colis.Symbolic.FilesystemSpec in
     match str with
     | "empty" -> fs_spec := empty
     | "simple" -> fs_spec := simple
     | "fsh" -> fs_spec := fsh
     | _ -> raise (Arg.Bad "only filesystems `empty', `simple', and `fsh' are known"))

let speclist =
  [ "--run",          Arg.Unit (set_action Run),         " Concrete execution (default)";
    "--run-symbolic", Arg.Unit (set_action RunSymbolic), " Symbolic execution";
    "--symbolic-fs",  Arg.String set_symbolic_fs,        " Name of the initial symbolic filesystem (default: empty)";
    "--print-colis",  Arg.Unit (set_action PrintColis),  " Print the CoLiS script";
    "--print-shell",  Arg.Unit (set_action PrintShell),  " Print the Shell script";
    "--realworld",    Arg.Set realworld,                 " Use system utilities in concrete execution";
    "--shell",        Arg.Unit (set_source Shell),       " Use the shell parser (default)";
    "--colis",        Arg.Unit (set_source Colis),       " Use the colis parser" ]
  |> Arg.align

let usage =
  sprintf
    "Usage: %s [--run [--realworld] | --run-symbolic | --print-colis | --print-shell] [--shell | --colis] FILE [ARGS]"
    Sys.argv.(0)

let main () =
  Arg.parse speclist set_file_or_argument usage;
  if !realworld && get_action () <> Run then
    raise (Arg.Bad "--realworld can only be specified with --run");

  let file = get_file () in
  let from_file =
    match get_source () with
    | Colis -> Colis.colis_from_file
    | Shell -> Colis.(shell_from_file ||> shell_to_colis)
  in
  let program =
    try from_file file
    with
    | Colis.ParseError (msg, pos) ->
       let print_position fmt pos =
         let open Lexing in
         fprintf fmt "%s:%d:%d" pos.pos_fname
           pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
       in
       eprintf "Syntax error at %a%s@." print_position pos (if msg = "" then "" else ": "^msg);
       exit 2
    | Colis.ConversionError msg ->
       eprintf "Conversion error: %s@." msg;
       exit 3
    | Sys_error msg ->
      eprintf "System error: %s@." msg;
      exit 4
  in
  let argument0 = file in
  let arguments = get_arguments () in
  match get_action () with
  | Run ->
     (
      Colis.run ~argument0 ~arguments program
     )
  | RunSymbolic ->
     (
      let fs_spec = get_symbolic_fs () in
      Colis.run_symbolic ~argument0 ~arguments fs_spec program
     )
  | PrintColis ->
     (
       Colis.print_colis program;
       exit 0
     )
  | PrintShell ->
     (
       eprintf "Printing Shell is not supported yet.@.";
       exit 3
     )

let () =
  try
    main ()
  with
    Arg.Bad msg ->
    eprintf "Error: %s@." msg;
    Arg.usage speclist usage;
    exit 3
