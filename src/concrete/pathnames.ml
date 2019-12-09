let starts_on_slash s = String.length s > 0 && s.[0]='/'

let ends_on_slash s = let n = String.length s in n > 0 && s.[n-1]='/'

let remove_dirs s =
  try
    let last_slash_ind = String.rindex s '/'
    in String.sub s (last_slash_ind+1) ((String.length s)-last_slash_ind-1)
  with
    Not_found -> s

let is_dir_prefix p1 p2 =
  (* check whether [p1^'/'] is a prefix of [p2] *)
  let rec forall_from_to lower upper pred =
    (* check [(pred lower) && .... && (pred upper)] *)
    if lower > upper then true
    else pred lower && forall_from_to (lower+1) upper pred
  in
  let n1 = String.length p1
  and n2 = String.length p2
  in
  if n1+1 >= n2
  then
    false
  else
    p2.[n1]='/' && forall_from_to 0 (n1-1) (function i -> p1.[i]=p2.[i])

