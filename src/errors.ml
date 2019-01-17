exception FileError of string
exception ParseError of string * Lexing.position
exception ConversionError of string

exception UnsupportedUtility of string
exception UnsupportedArgument of string * string
