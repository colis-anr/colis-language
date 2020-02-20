open SymbolicUtility.Symbolic

let name = "update-alternatives"

let interprete ctx =
  let name = "update-alternatives" in
  let rec aux = function
    | [] ->
      (* TODO: return error state *)
      error ~utility:name "no sub-command found"
    | "--quiet" :: rem->
      fun st -> aux rem (print_utility_trace (name ^ ": ignored option --quiet") st)
    | arg :: _ ->
      error ~utility:name ("unsupported argument: " ^ arg)
  in
  aux ctx.args
