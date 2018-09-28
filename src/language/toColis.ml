open Syntax__Syntax

let statement fmt = function
  | SNop -> Format.pp_print_string fmt "NOP"
  | _ -> failwith "Not implemented: ToColis.statement"
