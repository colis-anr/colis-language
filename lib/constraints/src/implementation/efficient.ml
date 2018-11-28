type t = unit

let true_ = ()

let eq _x _y _c = assert false
let neq _x _y _c = assert false

let feat _x _f _y _c = assert false
let nfeat _x _f _y _c = assert false

let abs _x _f _c = assert false
let nabs _x _f _c = assert false

let reg _x _c = assert false
let nreg _x _c = assert false

let dir _x _c = assert false
let ndir _x _c = assert false

let fen _x _fs _c = assert false
let nfen _x _fs _c = assert false

let sim _x _fs _y _c = assert false
let nsim _x _fs _y _c = assert false

let quantify_over _x _c = assert false

let pp _ _ = assert false
