module C = Syntax__Syntax

let on_located = Morsmall.Location.on_located

let special_builtins = [
    "break"; ":"; "continue"; "."; "eval"; "exec";
    "exit"; "export"; "readonly"; "return"; "set";
    "shift"; "times"; "trap"; "unset" ]
(* cd is not in that list because it is technically not a special built-in! *)
   
let parse _filename : C.instruction =
  failwith "FromShell.parse: not implemented"
