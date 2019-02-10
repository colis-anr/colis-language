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

let prune_init_state = ref false
let loop_limit = ref 10
let stack_size = ref 10

let get_symbolic_fs, set_symbolic_fs =
  let fs_spec = ref Colis.Symbolic.FilesystemSpec.empty in
  (fun () -> !fs_spec),
  (fun str ->
     let open Colis.Symbolic.FilesystemSpec in
     match str with
     | "empty" -> fs_spec := empty
     | "simple" -> fs_spec := simple
     | "fhs" -> fs_spec := fhs
     | _ -> raise (Arg.Bad "only filesystems `empty', `simple', and `fsh' are known"))

let speclist =
  let open Arg in
  let open Colis.Options in
  align [
    "--run",                       Unit (set_action Run),         " Concrete execution (default)";
    "--run-symbolic",              Unit (set_action RunSymbolic), " Symbolic execution";
    "--shell",                     Unit (set_source Shell),       " Use the shell parser (default)";
    "--colis",                     Unit (set_source Colis),       " Use the colis parser" ;
    "--external-sources",          Set_string external_sources,   "DIR Import absolute sources from DIR";
    "--print-colis",               Unit (set_action PrintColis),  " Print the CoLiS script";
    "--print-shell",               Unit (set_action PrintShell),  " Print the Shell script";
    "--realworld",                 Set real_world,                 " Use system utilities in concrete execution";
    "--symbolic-fs",               String set_symbolic_fs,        "FS Name of the initial symbolic filesystem in symbolic execution (values: empty, simple, fhs, default: empty)";
    "--prune-init-state",          Set prune_init_state,          " Prune the initial state in symbolic execution";
    "--loop-limit",                Int ((:=) loop_limit),         sprintf "LIMIT Set limit for symbolic execution of while loops to LIMIT (default: %d)" !loop_limit;
    "--stack-size",                Int ((:=) stack_size),         sprintf "SIZE Set the stack size for symbolic execution to LIMIT (default: %d)" !stack_size;
    "--print-states",              String ((:=)print_states_dir), "DIR Save symbolic states as dot files in directory DIR";
    "--fail-on-unknown-utilities", Set fail_on_unknown_utilities, " Unknown utilities kill the interpreter";
  ]

let usage =
  sprintf
    ("Usage: %s [--run <run-options> | --run-symbolic <symbolic-run-options> | --print-colis | --print-shell] <parsing-options> FILE [ARGS]\n"^^
     "       <run-options>: [--realworld |  --fail-on-unknown-utilities]\n"^^
     "       <symbolic-run-options>: [--symbolic-fs FS] [--prune-init-state] [--loop-boundary] [--fail-on-unknown-utilities] [--print-states DIR]\n"^^
     "       <parsing-options>: [--shell [--external-sources DIR] | --colis]")
    Sys.argv.(0)

let main () =
  (* Parse command line *)

  Arg.parse speclist set_file_or_argument usage;
  if !Colis.Options.real_world && get_action () <> Run then
    raise (Arg.Bad "--realworld can only be specified with --run");
  if !Colis.Options.fail_on_unknown_utilities && (get_action () <> Run && get_action () <> RunSymbolic) then
    raise (Arg.Bad "--fail-on-unknown-utilities can only be specified with --run or --run-symbolic");
  if !prune_init_state && get_action () <> RunSymbolic then
    raise (Arg.Bad "--prune-init-state can only be specified with --run-symbolic");

  (* Read input file *)

  let file = get_file () in
  let program =
    (match get_source () with
     | Colis -> Colis.parse_colis_file
     | Shell -> Colis.parse_shell_file)
      file
  in

  let argument0 = file in
  let arguments = get_arguments () in
  match get_action () with
  | Run ->
     Colis.run ~argument0 ~arguments program
  | RunSymbolic ->
     let fs_spec = get_symbolic_fs () in
     Colis.run_symbolic
       ~prune_init_state:!prune_init_state
       ~loop_limit:!loop_limit
       ~stack_size:!stack_size
       ~fs_spec
       ~argument0 ~arguments program
  | PrintColis ->
     Colis.print_colis program;
  | PrintShell ->
     eprintf "Printing Shell is not supported yet.@.";
     exit 9 (* FIXME *)

let () =
  let open Colis.Errors in

  try
    main ();
    exit 0
  with

  (* Error in command line parsing. *)
  | Arg.Bad msg ->
     eprintf "Error in command line parsing: %s@." msg;
     Arg.usage speclist usage;
     exit 3

  (* Error while reading file. *)
  | FileError msg ->
     eprintf "File error: %s@." msg;
     exit 4

  (* Error in parsing (shell or colis). *)
  | ParseError (msg, pos) ->
     let print_position fmt pos =
       let open Lexing in
       fprintf fmt "%s:%d:%d" pos.pos_fname
         pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
     in
     eprintf "Syntax error at %a%s@." print_position pos (if msg = "" then "" else ": "^msg);
     exit 5

  (* Conversion error. *)
  | ConversionError msg ->
     eprintf "Conversion error: %s@." msg;
     exit 6

  | UnsupportedUtility (name, msg) ->
     eprintf "%s: %s@." name msg;
     exit 7

  | UnsupportedArgument (name, msg, arg) ->
     eprintf "%s: %s: %s@." name msg arg;
     exit 8
