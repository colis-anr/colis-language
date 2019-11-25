exception FileError of string
exception ParseError of string * Lexing.position
exception ConversionError of Morsmall.Location.position * string

exception Unsupported of string * string
let unsupported ~utility msg = raise (Unsupported (utility, msg))

exception CpuTimeLimitExceeded
exception MemoryLimitExceeded
