open SymbolicUtility
open Semantics__Result

let name = "dpkg"

let interprete ctx =
  let utility = "dpkg" in
  let aux = function
    | ["-L"; pkg_name] ->
      if Colis_internals.Options.get_package_name () = pkg_name then
        (
          let str =
            Colis_internals.Options.get_contents ()
            |> String.concat "\n"
          in
          fun sta ->
            let sta = print_stdout ~newline:true str sta in
            [sta, Ok true]
        )
      else
        unsupported ~utility "-L about an other package"

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
