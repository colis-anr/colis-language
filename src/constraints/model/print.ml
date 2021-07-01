open Common

let rec int_list_display = function
  |[] -> ()
  |h::t -> Printf.fprintf out_f_l "%d, " h;
           Format.printf "%d, " h;
           int_list_display t

let rec str_list_display = function
  |[] -> ()
  |h::t -> Printf.fprintf out_f_l "%s, " h;
           Format.printf "%s, " h;
           str_list_display t

let kind_to_str = function
  |Dir -> "Dir"
  |Reg -> "Reg"
  |Other -> "Other"
  |Unknown -> "Unknown"

let node_display {var_l = var_l_ ;feat = feat_ ;notfeat = notfeat_; equality= equality_;sim= sim_;fen_p =fen_p_;fen = fen_;id=id_;kind = kind_} : unit = 
    
    let feat_ = FMap.bindings feat_ in
    let var_display var_l_ = Format.printf "[" ;
                       int_list_display (VSet.elements var_l_);
                       Format.printf "]\t";
    in

    let rec feat_display feat_ = 
      match feat_ with 
      |[] -> ()
      |(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" f_1 v_1;
                       feat_display t 
    in 
    let rec notfeat_display notfeat_ = 
      match notfeat_ with 
      |[] -> ()
      |(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" f_1 v_1;
                       notfeat_display t 
    in
    
    let fen_ = FSet.elements fen_ in
    let rec fen_display fen_ = 
      match fen_ with 
      |[] -> ()
      | h::t -> Format.printf "[%s]\t" h;
                fen_display t
    in 

    let rec equality_display equality_ = 
      match equality_ with 
      |[] -> ()
      |(f_1,v_1)::t -> Format.printf "[" ;
                       str_list_display (FSet.elements f_1);
                       Format.printf " --> %d]\t" v_1;
                equality_display t
    in

    let rec sim_display sim_ = 
      match sim_ with 
      |[] -> ()
      |(f_1,v_1)::t -> Format.printf "[" ;
                       str_list_display (FSet.elements f_1);
                       Format.printf " --> %d]\t" v_1;
                sim_display t
    in

    Format.printf "Variable List:\n";
    var_display var_l_;
    Format.printf "\nFeatures:\n";
    feat_display feat_;
    Format.printf "\nNot-Features:\n";
    notfeat_display notfeat_;
    Format.printf "\nFen Features(present: %b):\n" fen_p_;
    fen_display fen_;
    Format.printf "\nEquality:\n";
    equality_display equality_;
    Format.printf "\nSimilarity:\n";
    sim_display sim_;
    Format.printf "\nInode: %s \n" id_;
    Format.printf "\nKind: %s \n" (kind_to_str kind_)

let var_map_display var_map = 
    let var_map = VarMap.bindings var_map in
    let rec helper var_map = 
      match var_map with 
      |[] -> ()
      |(v_1,n_1)::t -> Format.printf "\n\n\t\tNODE(VAR) : %d\n" v_1;
                    node_display n_1;
                    helper t
    in 
    helper var_map

let print_Atom (x:atom) y  =
  match x with
  | Eq(v1,v2) ->  Printf.fprintf out_f_l " %s(Eq(%d,%d)) " y v1 v2;
                  Format.printf " %s(Eq(%d,%d)) " y v1 v2

  | Feat(v1,f,v2) -> Printf.fprintf out_f_l " %s(Feat(%d,%s,%d)) " y v1 f v2;
                  Format.printf " %s(Feat(%d,%s,%d)) " y v1 f v2

  | Abs(v1,f) ->  Printf.fprintf out_f_l " %s(Abs(%d,%s)) " y v1 f;
                  Format.printf " %s(Abs(%d,%s)) " y v1 f

  | Kind(v1,k) -> Printf.fprintf out_f_l " %s(Kind(%d,%s)) " y v1 (kind_to_str k);
                  Format.printf " %s(Kind(%d,%s)) " y v1 (kind_to_str k)

  | Fen(v1,f) ->Printf.fprintf out_f_l " %s(Fen(%d,[" y v1;
                Format.printf " %s(Fen(%d,[" y v1;
                (str_list_display f);
                Printf.fprintf out_f_l "])) "; 
                Format.printf "])) "
  
  | Sim(v1,f,v2) -> Printf.fprintf out_f_l " %s(Sim(%d,[" y v1;
                Format.printf " %s(Sim(%d,[" y v1;
                (str_list_display f);
                Printf.fprintf out_f_l "],%d)) " v2; 
                Format.printf "],%d)) " v2

  | Eqf(v1,f,v2) ->  Printf.fprintf out_f_l " %s(Eqf(%d,[" y v1;
                Format.printf " %s(Eqf(%d,[" y v1;
                (str_list_display f);
                Printf.fprintf out_f_l "],%d)) " v2; 
                Format.printf "],%d)) " v2

  | Maybe(v1,f,v2) -> Printf.fprintf out_f_l " %s(Maybe(%d,%s,%d)) " y v1 f v2;
                Format.printf " %s(Maybe(%d,%s,%d)) " y v1 f v2

let rec print_clause (x:literal list)  =
  match x with
  | [] -> Printf.fprintf out_f_l "\n\n";Format.printf "\n\n"
  | Pos a::t -> print_Atom a "Pos"; print_clause t
  | Neg a::t -> print_Atom a "Neg"; print_clause t
