module Syntax = struct
  (* wrapped in a opened module not to polute the syntax helpers *)

  type split = Syntax__Syntax.split =
    | Split
    | DontSplit

  and string_expression = Syntax__Syntax.string_expression =
    | SLiteral of string
    | SVariable of string
    | SSubshell of instruction
    | SConcat of string_expression * string_expression
    | SArgument of (Syntax__Nat.nat [@opaque])

  and list_expression = (* no extracted type *)
    (string_expression * split) list

  and function_definition = (* no extracted type *)
    string * instruction

  and return_code = Syntax__Syntax.return_code =
    | RSuccess
    | RFailure
    | RPrevious

  and instruction = Syntax__Syntax.instruction =
    | IAssignment of string * string_expression
    | ISequence of instruction * instruction
    | ISubshell of instruction
    | IIf of instruction * instruction * instruction
    | INot of instruction
    | IPipe of instruction * instruction
    | IWhile of instruction * instruction
    | INoOutput of instruction
    | IForeach of string * list_expression * instruction
    | ICallUtility of string * list_expression
    | ICallFunction of string * list_expression
    | IReturn of return_code
    | IExit of return_code
    | IShift of (Syntax__Nat.nat [@opaque]) option
    | IExport of string
    | ICd of string_expression

  and program = Syntax__Syntax.program =
    { function_definitions : function_definition list;
      instruction : instruction }

  [@@deriving
    visitors { variety = "map" },
    visitors { variety = "reduce" }
    ]
end
open Syntax

class virtual ['self] map = object
  inherit ['self] Syntax.map
end
class virtual ['self] reduce = object
  inherit ['self] Syntax.reduce
end

let sconcat_l = function
  | [] -> SLiteral ""
  | s :: ss -> List.fold_left (fun s1 s2 -> SConcat (s1, s2)) s ss

let lliteral ?(split=false) lit =
  (SLiteral lit, if split then Split else DontSplit)

let isequence_l = function
  | [] -> failwith "isequence_l"
  | i :: is -> List.fold_left (fun i1 i2 -> ISequence (i1, i2)) i is

let icallutility name lexpr = ICallUtility(name, lexpr)

let icolon = ICallUtility (":", [])
let itrue = ICallUtility ("true", [])
let ifalse = ICallUtility ("false", [])

let ior (i1, i2) = IIf (i1, icolon, i2)
let iand  (i1, i2) = IIf (i1, i2, INot icolon)

let program ?(function_definitions=[]) instruction =
  { function_definitions ; instruction }
