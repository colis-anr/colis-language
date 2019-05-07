type t = unit

let true_ = ()

let eq _x _y _c = assert false
let neq _x _y _c = assert false
let feat _x _f _y _c = assert false
let nfeat _x _f _y _c = assert false
let abs _x _f _c = assert false
let nabs _x _f _c = assert false
let fen _x _fs _c = assert false
let nfen _x _fs _c = assert false
let sim _x _fs _y _c = assert false
let nsim _x _fs _y _c = assert false

let reg _x _c = assert false
let nreg _x _c = assert false
let dir _x _c = assert false
let ndir _x _c = assert false
let block _x _c = assert false
let nblock _x _c = assert false
let symlink _x _c = assert false
let nsymlink _x _c = assert false
let char _x _c = assert false
let nchar _x _c = assert false
let pipe _x _c = assert false
let npipe _x _c = assert false
let sock _x _c = assert false
let nsock _x _c = assert false

let quantify_over _x _c = assert false

let pp _ _ = assert false
let pp_as_dot ~name _ _ = ignore name; assert false
