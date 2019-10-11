type t = int

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i

let fresh2 () = (fresh (), fresh ())
let fresh3 () = (fresh (), fresh (), fresh ())
let fresh4 () = (fresh (), fresh (), fresh (), fresh ())
let fresh5 () = (fresh (), fresh (), fresh (), fresh (), fresh ())

let compare = compare
let equal m n = compare m n = 0

module Self = struct
  type s = t
  type t = s

  let compare = compare
end

module Map = Map.Make(Self)
