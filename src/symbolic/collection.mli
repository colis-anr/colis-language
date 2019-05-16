type 'a t
val empty : 'a t
val add : 'a -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val filter : ('a -> bool) -> 'a t -> 'a t
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
val union : 'a t -> 'a t -> 'a t
val bind : ('a -> 'b t) -> 'a t -> 'b t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val size : 'a t -> int
