module SMap = Map.Make (String)

type 'a env = {map: 'a SMap.t; default: 'a}

let empty default = {map=SMap.empty; default}
let get env id = try SMap.find id env.map with Not_found -> env.default
let set env id value = {env with map=SMap.add id value env.map}
let filter p env = {env with map=SMap.filter p env.map}
let map f default env = {map=SMap.map f env.map; default}
let elements env = SMap.fold (fun k v t -> (k, v) :: t) env.map []
let to_map env = env.map

let filter_var_env (var_exported: 'a -> bool) (var_value: 'a -> 'b option) (env : 'a env) : 'b SMap.t =
  let open SMap in
  env.map |>
  filter (fun _ -> var_exported) |>
  map var_value |>
  filter (fun _ -> (<>) None) |>
  map (function Some x -> x | _ -> assert false)
