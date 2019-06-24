open SymbolicUtility

(** {2 Building a description of the command line arguments} *)

type 'a t

val fun_ : (context -> 'c) -> context -> 'c t

val flag : string list -> bool t
(** Define a flag that does not take an argument. It can combine with others. *)

val ($) : ('a -> 'b) t -> 'a t -> 'b t

(** {2 Evaluating a command} *)

val eval : utility:string -> context -> (string list -> utility) t -> utility
