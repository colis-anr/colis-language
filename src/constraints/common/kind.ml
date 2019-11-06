type t = Reg | Dir | Char | Sock | Pipe | Symlink | Block
[@@deriving yojson]

let pp fmt kind =
  Format.pp_print_string fmt
    (match kind with
     | Reg -> "reg"
     | Dir -> "dir"
     | Char -> "char"
     | Sock -> "sock"
     | Pipe -> "pipe"
     | Symlink -> "symlink"
     | Block -> "block")

let equal = (=)
let compare = compare

let all =
  [Reg; Dir; Char; Sock; Pipe; Symlink; Block]
  |> List.sort compare

let nb_all = List.length all
