open Syntax__Syntax

let rec statement fmt = function
  | SNop -> Format.pp_print_string fmt "NOP"
  | _ -> failwith "Not implemented: ToColis.statement"
