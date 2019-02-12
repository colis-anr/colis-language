
type t = node list
and node = Dir of string * t

let node name children : node =
  Dir (name, children)

open Constraints

let rec compile_node x = function
  | Dir (name, children) ->
    let f = Feat.from_string name in
    let open Clause in
    exists ~hint:name @@ fun y ->
    List.fold_left and_ (feat x f y & dir y)
      (List.map (compile_node y) children)

let compile ~root nodes =
  let clause =
    let open Clause in
    List.fold_left and_ (dir root)
      (List.map (compile_node root) nodes)
  in
  clause

let empty : t = []

let simple : t = [
  node "bin" [];
  node "sbin" [];
  node "usr" [
    node "bin" [];
    node "sbin" [];
    node "local" [
      node "lib" [];
    ];
  ];
  node "etc" [];
  node "usr" [
    node "lib" [];
  ];
  node "var" [
    node "lib" [];
  ]
]

(* TODO extended to FSH 3.0 *)
let fhs : t = [
  node "bin" [];
  node "boot" [];
  node "dev" [];
  node "etc" [];
  node "lib" [];
  node "media" [];
  node "mnt" [];
  node "opt" [];
  node "proc" [];
  node "root" [];
  node "sbin" [];
  node "srv" [];
  node "sys" [];
  node "tmp" [];
  node "usr" [
    node "bin" [];
    node "local" [];
    node "doc" [];
    node "etc" [];
    node "include" [];
    node "lib" [];
    node "local" [
      node "bin" [];
      node "doc" [];
      node "etc" [];
      node "lib" [];
      node "include" [];
    ];
    node "share" [
      node "doc" [];
    ];
    node "src" [];
  ];
  node "var" [
    node "cache" [];
    node "lib" [];
    node "local" [];
    node "log" [];
    node "run" [];
    node "spool" [
      node "cron" [];
    ];
    node "tmp" [];
  ];
]
