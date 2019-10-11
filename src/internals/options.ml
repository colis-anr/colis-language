let fail_on_unknown_utilities = ref false
let real_world = ref false
let external_sources = ref ""
let print_states_dir = ref ""

let cpu_time_limit = ref infinity
let memory_limit = ref infinity

let set_memory_limit s =
  let l = String.length s in
  if l = 0 then
    ();
  let m =
    match s.[l-1] with
    | 'g' | 'G' -> 1073741824.
    | 'm' | 'M' -> 1048576.
    | 'k' | 'K' -> 1024.
    | _ -> 1.
  in
  let s =
    if m = 1. then
      s
    else
      String.sub s 0 (l - 1)
  in
  memory_limit := float_of_string s *. m
