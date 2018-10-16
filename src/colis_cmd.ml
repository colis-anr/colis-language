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

let speclist =
  [ "--run",          Arg.Unit (set_action Run),         " Concrete execution (default)";
    "--run-symbolic", Arg.Unit (set_action RunSymbolic), " Symbolic execution";
    "--print-colis",  Arg.Unit (set_action PrintColis),  " Print the CoLiS script";
    "--print-shell",  Arg.Unit (set_action PrintShell),  " Print the Shell script";
    "--realworld",    Arg.Set realworld,                 " Use system utilities in concrete execution";
    "--shell",        Arg.Unit (set_source Shell),       " Use the shell parser (default)";
    "--colis",        Arg.Unit (set_source Colis),       " Use the colis parser" ]
  |> Arg.align

let usage =
  Format.sprintf
    "Usage: %s [--run [--realworld] | --run-symbolic | --print-colis | --print-shell] [--shell | --colis] FILE"
    Sys.argv.(0)

let main () =
  Arg.parse speclist set_file_or_argument usage;
  if !realworld && get_action () <> Run then
    raise (Arg.Bad "--realworld can only be specified with --run");

  let program =
    get_file ()
    |> match get_source () with
       | Colis -> Colis.parse_colis
       | Shell -> Colis.(parse_shell ||> shell_to_colis)
  in
  match get_action () with
  | Run ->
     (
       Colis.run ~arguments:(get_arguments ()) program
     )
  | RunSymbolic ->
     (
       Format.eprintf "Symbolic execution is not supported yet.@.";
       exit 3
     )
  | PrintColis ->
     (
       Colis.print_colis program
     )
  | PrintShell ->
     (
       Format.eprintf "Printing Shell is not supported yet.@.";
       exit 3
     )

let () =
  try
    main ()
  with
    Arg.Bad msg ->
    Format.eprintf "Error: %s@." msg;
    Arg.usage speclist usage;
    exit 3
