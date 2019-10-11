let rec insert_uniq_sorted cmp e = function
  | [] -> [e]
  | h :: q ->
    match cmp h e with
    | c when c < 0 -> h :: insert_uniq_sorted cmp e q
    | c when c = 0 -> h :: q
    | _            -> e :: h :: q
