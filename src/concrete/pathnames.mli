(* operations on path names *)

(* check whether the first symbol of the path name is the symbol '/' *)
val starts_on_slash: string -> bool

(* check whether the last symbol of the path name is the symbol '/' *)
val ends_on_slash: string -> bool

(* return the path name with any leading directory components removed *)
val remove_dirs: string -> string
