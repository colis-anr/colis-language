exception FileError of string
exception ParseError of string * Lexing.position
exception ConversionError of string

exception Unsupported of string * string
let unsupported ~utility msg = raise (Unsupported (utility, msg))

exception CPU_time_limit_exceeded
