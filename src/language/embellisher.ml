open Colis_internals.Ext
open Syntax__Syntax

let embellish colis =
  let visitor = object
    inherit [_] SyntaxHelpers.map as super

    method! visit_ICallUtility () name args =
      match name with
      (* Rewrite "[ ... ]" into "test ..." *)
      | "[" when List.ft_opt args = Some (SLiteral "]", DontSplit) ->
        super#visit_ICallUtility () "test" (List.bd args)

      | _ -> super#visit_ICallUtility () name args
  end in
  visitor#visit_program () colis
