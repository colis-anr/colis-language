open SymbolicUtility

let name = "dpkg"

let interprete ctx =
  let utility = "dpkg" in
  let aux = function
    | ["-L"; _pkg_name] ->
      unsupported ~utility "support for -L not yet implemented"
    | "-L" :: _ ->
      unsupported ~utility "option -L expects exactly one argument"
    | ["--compare-versions"; _v1; _v2] ->
      unsupported ~utility "support for --compare-versions not yet implemented"
    | "--compare-versions" :: _ ->
      unsupported ~utility "option --compare-versions expects exactly two arguments"
    | [] ->
      (* TODO: return error state *)
      unsupported ~utility "no argument found"
    | arg :: _ ->
      unsupported ~utility ("unsupported argument: " ^ arg)
  in
  aux ctx.args
