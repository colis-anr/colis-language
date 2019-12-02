let fail_on_unknown_utilities = ref false
let real_world = ref false
let external_sources = ref ""
let print_states_dir = ref ""

let cpu_time_limit = ref infinity
let memory_limit = ref max_int (* memory limit stored in words *)

let set_memory_limit s =
  let l = String.length s in
  if l = 0 then
    ();
  let m =
    match s.[l-1] with
    | 'g' | 'G' -> 1073741824
    | 'm' | 'M' -> 1048576
    | 'k' | 'K' -> 1024
    | _ -> 1
  in
  let s =
    if m = 1 then
      s
    else
      String.sub s 0 (l - 1)
  in
  memory_limit := int_of_string s * m * 8 / Sys.word_size

type 'a gs =
  { getter : unit -> 'a ;
    setter : 'a -> unit ;
    wither : 'b. 'a -> (unit -> 'b) -> 'b }

let make_getters_setters str =
  let holder = ref None in
  let getter () =
    match !holder with
    | None -> raise (Arg.Bad (str ^ " has not been set yet"))
    | Some content -> content
  in
  let setter content =
    match !holder with
    | None -> holder := Some content
    | Some _ -> raise (Arg.Bad (str ^ " has already been set"))
  in
  let wither content f =
    let old = !holder in
    holder := Some content;
    let res = try Ok (f ()) with exn -> Error exn in
    holder := old;
    match res with Ok res -> res | Error exn -> raise exn
  in
  { getter ; setter ; wither }

let (gs : string list gs) = make_getters_setters "Contents"
let get_contents = gs.getter
let set_contents = gs.setter
let with_contents = gs.wither

let (gs : string gs) = make_getters_setters "Package name"
let get_package_name = gs.getter
let set_package_name = gs.setter
let with_package_name = gs.wither
