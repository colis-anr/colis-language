exception FileError of string
exception ParseError of string * Lexing.position
exception ConversionError of Morsmall.Location.position * string

exception Unknown_behaviour of string * string

exception CpuTimeLimitExceeded
exception MemoryLimitExceeded
