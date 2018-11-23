type 'a m = 'a option

val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
