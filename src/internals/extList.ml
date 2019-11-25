let rec bd = function
  | [] -> failwith "bd"
  | [_] -> []
  | h :: t -> h :: bd t

let rec ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _ :: t -> ft t

let rec ft_opt = function
  | [] -> None
  | [e] -> Some e
  | _ :: t -> ft_opt t
