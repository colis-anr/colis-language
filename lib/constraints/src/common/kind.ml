type t = Reg | Dir

let pp fmt = function
  | Reg -> Format.pp_print_string fmt "reg"
  | Dir -> Format.pp_print_string fmt "dir"

let equal = (=)
let compare = compare

let all = [Reg; Dir]
