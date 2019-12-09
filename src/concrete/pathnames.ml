let starts_on_slash s = String.length s > 0 && s.[0]='/'

let ends_on_slash s = let n = String.length s in n > 0 && s.[n-1]='/'

let remove_dirs s =
  try
    let last_slash_ind = String.rindex s '/'
    in String.sub s (last_slash_ind+1) ((String.length s)-last_slash_ind-1)
  with
    Not_found -> s
;;
