module Loglevel = struct
  type t =
    | Debug
    | Info
    | Warning

  let to_string = function
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warning -> "WARNING"

  let to_int = function
    | Debug -> 0
    | Info -> 1
    | Warning -> 2
end

let current = ref Loglevel.Info

let log level f =
  if Loglevel.to_int level >= Loglevel.to_int !current then
    (
      Format.eprintf "[%s]" (Loglevel.to_string level);
      f Format.eprintf;
      Format.eprintf "@."
    )

let debug f = log Debug f
let info f = log Info f
let warn f = log Warning f
