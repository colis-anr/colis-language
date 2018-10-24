let utility = ref "colis"
let directory = ref Filename.current_dir_name
let sh_only = ref false
              
let speclist =
  [ "--utility", Arg.Set_string utility, "UTIL Set the utility to test (default: colis)";
    "--directory", Arg.Set_string directory, "DIR Set the directory in which the tests can be found (default: .)" ;
    "--sh-only", Arg.Set sh_only, " Only test .sh files (default: false)" ]
  |> Arg.align

let anon_fun _ =
  raise (Arg.Bad "no anonymous argument allowed")
  
let usage_msg =
  Format.sprintf
    "Usage: %s [--utility UTIL] [--directory DIR]"
    Sys.argv.(0)

let parse_cmd_line () =
  Arg.parse speclist anon_fun usage_msg
