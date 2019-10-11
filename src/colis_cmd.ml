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

let get_file, get_arguments, set_file_or_argument, add_argument =
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
    | Some _ -> args := new_file_or_arg :: !args),
  (fun arg -> args := arg :: !args)

let prune_init_state = ref false
let loop_limit = ref 10
let stack_size = ref 10

let get_symbolic_fs, add_symbolic_fs =
  let open Colis.Symbolic.FilesystemSpec in
  let fs_spec = ref empty in
  (fun () -> !fs_spec),
  (fun filename ->
     try
       let cin = open_in filename in
       fs_spec := add_channel cin !fs_spec;
       close_in cin
     with Sys_error msg ->
       raise (Arg.Bad msg))

let set_var, get_vars =
  let vars = ref [] in
  let set str =
    try
      let ix = String.index str '=' in
      let var = String.sub str 0 ix in
      let val_ = String.sub str (ix+1) (String.length str-ix-1) in
      vars := (var, val_) :: !vars
    with Not_found ->
      raise (Arg.Bad "Variables have to be defined as VAR=VAL")
  in
  set, (fun () -> List.rev !vars)

let speclist =
  let open Arg in
  let open Colis.Internals.Options in
  align [
    "--run",                       Unit (set_action Run),         " Concrete execution (default)";
    "--run-symbolic",              Unit (set_action RunSymbolic), " Symbolic execution";
    "--shell",                     Unit (set_source Shell),       " Use the shell parser (default)";
    "--colis",                     Unit (set_source Colis),       " Use the colis parser" ;
    "--external-sources",          Set_string external_sources,   "DIR Import absolute sources from DIR";
    "--print-colis",               Unit (set_action PrintColis),  " Print the CoLiS script";
    "--print-shell",               Unit (set_action PrintShell),  " Print the Shell script";
    "--var",                       String set_var,                " VAR=VAL Set and export variable VAR to VAL in the interpreter";
    "--realworld",                 Set real_world,                " Use system utilities in concrete execution";
    "--add-symbolic-fs",           String add_symbolic_fs,        "FILE Add files and directories from FILE to the initially empty symbolic file system (One file or directory per line; directories end with '/')";
    "--prune-init-state",          Set prune_init_state,          " Prune the initial state in symbolic execution";
    "--loop-limit",                Int ((:=) loop_limit),         sprintf "LIMIT Set limit for symbolic execution of while loops to LIMIT (default: %d)" !loop_limit;
    "--cpu-time-limit",            Float (fun f -> Constraints_common.Log.cpu_time_limit := Some f),     "LIMIT Set CPU time limit for symbolic execution to LIMIT in seconds (default: none)";
    "--stack-size",                Int ((:=) stack_size),         sprintf "SIZE Set the stack size for symbolic execution to SIZE (default: %d)" !stack_size;
    "--print-states",              String ((:=)print_states_dir), "DIR Save symbolic states as dot files in directory DIR";
    "--fail-on-unknown-utilities", Set fail_on_unknown_utilities, " Unknown utilities kill the interpreter";
    "--",                          Rest add_argument,             "ARG... Pass all further arguments directly to the CoLiS interpreter";
  ]

let usage =
  sprintf
    ("Usage: %s <action> <syntax-opts> [--var VAR=VAL ...] FILE [--] [ARG...]\n"^^
     "       <action>: [--run <concrete-opts> | --run-symbolic <symbolic-opts> | --print-colis | --print-shell]\n"^^
     "       <concrete-opts>: [--realworld |  --fail-on-unknown-utilities]\n"^^
     "       <symbolic-opts>: [--symbolic-fs FS] [--prune-init-state] [--loop-boundary] [--fail-on-unknown-utilities] [--print-states DIR]\n"^^
     "       <syntax-opts>: [--shell [--external-sources DIR] | --colis]")
    Sys.argv.(0)

let main () =
  (* Parse command line *)

  Arg.parse speclist set_file_or_argument usage;
  if !Colis.Internals.Options.real_world && get_action () <> Run then
    raise (Arg.Bad "--realworld can only be specified with --run");
  if !Colis.Internals.Options.fail_on_unknown_utilities && (get_action () <> Run && get_action () <> RunSymbolic) then
    raise (Arg.Bad "--fail-on-unknown-utilities can only be specified with --run or --run-symbolic");
  if !prune_init_state && get_action () <> RunSymbolic then
    raise (Arg.Bad "--prune-init-state can only be specified with --run-symbolic");

  (* Read input file *)

  let file = get_file () in
  let program =
    (match get_source () with
     | Colis -> Colis.parse_colis_file
     | Shell -> Colis.parse_shell_file ~cmd_line_arguments:(get_arguments ()))
      file
  in

  let argument0 = file in
  let arguments = get_arguments () in
  let vars = get_vars () in
  match get_action () with
  | Run ->
     Colis.run ~argument0 ~arguments ~vars program
  | RunSymbolic ->
     let config = {
       Colis.prune_init_state = !prune_init_state;
       loop_limit = !loop_limit;
       stack_size = !stack_size;
     } in
     let fs_spec = get_symbolic_fs () in
     Colis.run_symbolic config fs_spec ~argument0 ~arguments ~vars program
  | PrintColis ->
     Colis.print_colis program;
  | PrintShell ->
     eprintf "Printing Shell is not supported yet.@.";
     exit 9 (* FIXME *)

let () =
  let open Colis.Internals.Errors in

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

  | Unsupported (utility, msg) ->
     eprintf "%s: %s@." utility msg;
     exit 7

  | Constraints_common.Log.CPU_time_limit_exceeded ->
     eprintf "CPU time limit exceeded@.";
     exit 11
