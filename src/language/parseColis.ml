if Array.length Sys.argv != 1 then
  exit 2

let filename = Array.get Sys.argv 0

let point_pos = String.rindex filename '.'

let ext = String.sub filename point_pos (String.length filename)

let statement =
  match ext with
  | "cls" ->
    FromColis.parse filename
  | "sh" ->
    FromShell.parse filename
  | _ ->
    Format.eprintf "Unknown extension: %s@." ext;
    exit 2

let () = Format.printf "%a@." ToColis.statement statement
