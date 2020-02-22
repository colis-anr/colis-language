open Format
open Colis

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
  let open FilesystemSpec in
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

let set_unknown_behaviour str =
  let open Internals.Options in
  match get_action () with
  | PrintColis | PrintShell ->
    raise (Arg.Bad "--unknown-utilities can only be specified with --run or --run-symbolic")
  | _ ->
    let behaviour =
      match str with
      | "EXCEPTION" -> Exception
      | "INCOMPLETE" -> Incomplete
      | "ERROR" -> Error
      | _ -> raise (Arg.Bad "Invalid value for unknown behaviour") in
    unknown_behaviour := behaviour

let speclist =
  let open Arg in
  let open Internals.Options in
  align [
    "--run",               Unit (set_action Run),         " Concrete execution (default)";
    "--run-symbolic",      Unit (set_action RunSymbolic), " Symbolic execution";
    "--shell",             Unit (set_source Shell),       " Use the shell parser (default)";
    "--colis",             Unit (set_source Colis),       " Use the colis parser" ;
    "--external-sources",  Set_string external_sources,   "DIR Import absolute sources from DIR";
    "--print-colis",       Unit (set_action PrintColis),  " Print the CoLiS script";
    "--print-shell",       Unit (set_action PrintShell),  " Print the Shell script";
    "--var",               String set_var,                " VAR=VAL Set and export variable VAR to VAL in the interpreter";
    "--realworld",         Set real_world,                " Use system utilities in concrete execution";
    "--add-symbolic-fs",   String add_symbolic_fs,        "FILE Add files and directories from FILE to the initially empty symbolic file system (One file or directory per line; directories end with '/')";
    "--prune-init-state",  Set prune_init_state,          " Prune the initial state in symbolic execution";
    "--loop-limit",        Int ((:=) loop_limit),         sprintf "LIMIT Set limit for symbolic execution of while loops to LIMIT (default: %d)" !loop_limit;
    "--cpu-time-limit",    Float ((:=) cpu_time_limit),   "LIMIT Set CPU time limit for symbolic execution to LIMIT in seconds (default: none)";
    "--memory-limit",      String set_memory_limit,       "LIMIT Set memory limit for symbolic execution to LIMIT in bytes (default: none)";
    "--stack-size",        Int ((:=) stack_size),         sprintf "SIZE Set the stack size for symbolic execution to SIZE (default: %d)" !stack_size;
    "--print-states",      String ((:=)print_states_dir), "DIR Save symbolic states as dot files in directory DIR";
    "--unknown-behaviour", String set_unknown_behaviour,  "BHV Behaviour for unknown utilities: raising an exception (by default), like an unsupported utility (incomplete behaviour), or as a colis-level error";
    "--",                  Rest add_argument,             "ARG... Pass all further arguments directly to the CoLiS interpreter";
  ]

let usage =
  sprintf
    ("Usage: %s <action> <syntax-opts> [--var VAR=VAL ...] FILE [--] [ARG...]\n"^^
     "       <action>: [--run <opts> <concrete-opts> | --run-symbolic <opts> <symbolic-opts> | --print-colis | --print-shell]\n"^^
     "       <opts>: [--unknown-behaviour EXCEPTION|UNSUPPORTED|ERROR]\n"^^
     "       <concrete-opts>: [--realworld]\n"^^
     "       <symbolic-opts>: [--symbolic-fs FS] [--prune-init-state] [--loop-boundary] [--print-states DIR]\n"^^
     "       <syntax-opts>: [--shell [--external-sources DIR] | --colis]")
    Sys.argv.(0)

let main () =
  (* Parse command line *)

  Arg.parse speclist set_file_or_argument usage;
  if !Internals.Options.real_world && get_action () <> Run then
    raise (Arg.Bad "--realworld can only be specified with --run");
  if !prune_init_state && get_action () <> RunSymbolic then
    raise (Arg.Bad "--prune-init-state can only be specified with --run-symbolic");

  (* Read input file *)

  let file = get_file () in
  let program =
    (match get_source () with
     | Colis -> Language.parse_colis_file
     | Shell -> Language.parse_shell_file ~cmd_line_arguments:(get_arguments ()))
      file
  in

  let argument0 = file in
  let arguments = get_arguments () in
  let vars = get_vars () in
  match get_action () with
  | Run ->
     Concrete.run ~argument0 ~arguments ~vars program
  | RunSymbolic ->
    let open SymbolicConstraints in
    let config = {
      prune_init_state = !prune_init_state;
      loop_limit = !loop_limit;
      stack_size = !stack_size;
    } in
    let fs_spec = get_symbolic_fs () in
    run config fs_spec ~argument0 ~arguments ~vars program
  | PrintColis ->
    Language.print_colis program;
  | PrintShell ->
    eprintf "Printing Shell is not supported yet.@.";
    exit 9 (* FIXME *)

let pp_lexing_position fmt pos =
  let open Lexing in
  fprintf fmt "File \"%s\", line %d, character %d"
    pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let pp_morsmall_position fmt pos =
  let open Morsmall.Location in
  let open Lexing in
  let sp = pos.start_p in
  let sc = sp.pos_cnum - sp.pos_bol + 1 in
  fprintf fmt "File \"%s\", line %d, characters %d-%d"
    sp.pos_fname sp.pos_lnum sc (pos.end_p.pos_cnum - pos.start_p.pos_cnum - sc) (* FIXME: +1? *)

let () =
  let open Internals.Errors in
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
     eprintf "%a: Syntax error%s%s@." pp_lexing_position pos (if msg = "" then "" else ": ") msg;
     exit 5

  (* Conversion error. *)
  | ConversionError (pos, msg) ->
     eprintf "%a: Conversion error: %s@." pp_morsmall_position pos msg;
     exit 6

  | Unknown_behaviour (utility, msg) ->
     eprintf "%s: %s@." utility msg;
     exit 7

  | CpuTimeLimitExceeded ->
     eprintf "CPU time limit exceeded@.";
     exit 11

  | MemoryLimitExceeded ->
    eprintf "Memory limit exceeded@.";
    exit 12
