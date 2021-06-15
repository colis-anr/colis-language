(*In here a single var_map exists which is modified by the functions*)
(*VARIOUS TYPES NEEDED*)
type feature = string
let compare = compare
(*let equal f1 f2 = compare f1 f2 = 0*)

module Feat = struct
  type t = feature
  let compare = compare
end

module FSet = Set.Make(Feat)
module FMap = Map.Make(Feat)


type var = int
let compare = compare
let equal m n = compare m n = 0

module Var = struct
  type t = var
  let compare = compare
end

module VarMap = Map.Make(Var)
module VSet = Set.Make(Var)

type kindt = Reg | Dir | Other | Unknown (*Unknown will be treated as Dir*)

type node = { var_l: VSet.t;
			 feat: var FMap.t;
       notfeat: (feature*var) list ; (*Not using Map as -x[f]y,-x[f]z can exists together*)
			 equality: (FSet.t*var) list; 
			 sim: (FSet.t*var) list;
			 fen : FSet.t; (*empty signifies no Fen specified so all allowed*)
       id : string;
       kind: kindt;
			 }

type fT = |Leaf
          |Node of (var * (fT FMap.t))

type atom =
  | Eq of var * var
  | Eqf of var * feature list * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * feature list
  | Sim of var * feature list * var
  | Kind of var * kindt

type var_map_type = node VarMap.t

type literal =
 |Pos of atom
 |Neg of atom

type clause = literal list

let var_map = ref VarMap.empty
let fBigSet = ref FSet.empty
let paths = ref []

(*VARIOUS FUNCTIONS NEEDED*)

(*let empty_node ():node = {var_l = VSet.empty;feat = FMap.empty;equality = [];notfeat=[];sim = [];fen = FSet.empty}*)
let empty_node v:node = {var_l = VSet.of_list [v];feat = FMap.empty;equality = [];notfeat=[];sim = [];fen = FSet.empty;id = "";kind = Unknown}

let fresh =
  let i = ref 0 in
  fun () -> incr i; !i  

let rec int_list_display = function
  |[] -> ()
  |h::t -> Format.printf " %d, " h;
           int_list_display t

let rec str_list_display = function
  |[] -> ()
  |h::t -> Format.printf " %s, " h;
           str_list_display t

let kind_to_str = function
  |Dir -> "Dir"
  |Reg -> "Reg"
  |Other -> "Other"
  |Unknown -> "Unknown"

let node_display {var_l = var_l_ ;feat = feat_ ;notfeat = notfeat_; equality= equality_;sim= sim_;fen = fen_;id=id_;kind = kind_} : unit = 
    
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
    Format.printf "\nFen Features:\n";
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

let rec create_empty_var_map clause  = 
  match clause with 
  |[] -> var_map := VarMap.add 0 (empty_node 0) (!var_map);   (*Var 0 is used to respresent absent mapping*)
        ()
  |Pos Feat(v1,f,v2)::t| Neg Feat(v1,f,v2)::t -> 
                       var_map := VarMap.add v1 (empty_node v1) (!var_map); 
                       var_map := VarMap.add v2 (empty_node v2) (!var_map); 
                       fBigSet := FSet.add f (!fBigSet);
                       create_empty_var_map t  

  |Pos Abs (v1,f)::t| Neg Abs (v1,f)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    fBigSet := FSet.add f (!fBigSet);
                    create_empty_var_map t 

  |Pos Eqf (v1,fl,v2)::t| Neg Eqf (v1,fl,v2)::t ->
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 

  |Pos Eq(v1,v2)::t| Neg Eq(v1,v2)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    create_empty_var_map t 

  |Pos Sim (v1,fl,v2)::t| Neg Sim (v1,fl,v2)::t->
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    var_map := VarMap.add v2 (empty_node v2) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 

  |Pos Fen(v1,fl)::t| Neg Fen(v1,fl)::t -> 
                    var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    fBigSet := FSet.union (FSet.of_list fl) (!fBigSet);
                    create_empty_var_map t 
  |Pos Kind(v1,_)::t| Neg Kind(v1,_)::t -> var_map := VarMap.add v1 (empty_node v1) (!var_map);
                    create_empty_var_map t        


let find_node v1 = 
  let a = VarMap.find_opt v1 !var_map in
  match a with
  | None -> failwith ("Could not find Var"^(string_of_int v1))
  | Some nod -> nod 

let add_abs_to_node atom  = 
  match atom with
  | Abs (v1,f) -> let v1_node = find_node v1 in 
          let absent_var = 0 in 
          if ((FMap.mem f v1_node.feat)&&(FMap.find_opt f v1_node.feat <> Some 0)) then failwith "Clash: tring to create x[f]abs when x[f]y exists"    
          else
          let new_node = {v1_node with feat = FMap.add f absent_var v1_node.feat} in
             let rec helper vl =
                match vl with 
                |[]-> ()
                |v1::t -> var_map := (VarMap.add v1 new_node !var_map);
                    helper t
             in
             (helper (VSet.elements new_node.var_l))
          (*All mappings from f to 0 are considered absent*)
  |_ -> failwith "add_abs_to_node is only for Abs"

let add_feat_to_node atom = 
  match atom with
  | Feat(v1,f,v2) when v2 = 0 -> add_abs_to_node (Abs(v1,f))
  | Feat(v1,f,v2) -> 
             let v1_node = find_node v1 in
             if (FMap.find_opt f v1_node.feat  = Some 0) then failwith ("Clash: tring to create %d[%s]%d when x[f]abs exists"^(string_of_int v1)^f^(string_of_int v2))
             else if (not(FSet.is_empty v1_node.fen) && not(FSet.mem f v1_node.fen)) then failwith "Clash: tring to create x[f]y when x[F] exists and f does not belong to F"
             else 
             let new_node = {v1_node with feat = FMap.add f v2 v1_node.feat} in
             let rec helper  vl =
                match vl with 
                |[]-> () 
                |v1::t -> var_map := (VarMap.add v1 new_node (!var_map));
                    helper t
             in
             (helper (VSet.elements new_node.var_l))
  |_ -> failwith "add_feat_to_node is only for Feat"

let rec no_feat_abs_to_node atom =
  match atom with
  |Feat(v1,f,v2) -> let v1_node = find_node v1 in
                    if((FMap.find_opt  f v1_node.feat) = (Some v2))
                    then failwith "Clash x[f]y and -x[f]y both present"
                    else
                    let new_node = {v1_node with notfeat =  (f,v2)::v1_node.notfeat} in
                    let rec helper  vl =
                       match vl with 
                       |[]-> () 
                       |v1::t -> var_map := (VarMap.add v1 new_node (!var_map));
                           helper t
                    in
                    (helper (VSet.elements new_node.var_l))

  |Abs(v1,f) -> no_feat_abs_to_node (Feat(v1,f,0));
                if((FMap.find_opt  f (find_node v1).feat) = None) then
                    let v_new = ((VarMap.cardinal !var_map) + 3) in
                    var_map := VarMap.add v_new (empty_node v_new) !var_map;
                    add_feat_to_node (Feat (v1,f,v_new))  (*BUG : ADD A FEATURE to make sure not absent*)
                else ()
  |_ -> failwith "no_feat_abs_to_node is only for Feat"

let add_kind_to_node atom =
  match atom with
  | Kind(v1,k) -> let v1_node = find_node v1 in
                  var_map := (VarMap.add v1 {v1_node with kind = k} !var_map); 
                  ()
  |_ -> failwith "add_equal_to_node is only for kind"

let add_equal_to_node atom = 
  match atom with
  | Eqf (v1,fl,v2) -> let v1_node = find_node v1 in
                      let v2_node = find_node v2 in
            let fl_v = (FSet.of_list fl,v2) in        
            var_map := (VarMap.add v1 {v1_node with equality = fl_v :: v1_node.equality} !var_map);
            let fl_v = (FSet.of_list fl,v1) in  
            var_map := (VarMap.add v2 {v2_node with equality = fl_v :: v2_node.equality} !var_map);
            ()
  |_ -> failwith "add_equal_to_node is only for Eqf"

let add_sim_to_node atom = 
  match atom with
  | Sim (v1,fl,v2) -> let v1_node = find_node v1 in
                      let v2_node = find_node v2 in
            let fl_v = (FSet.of_list fl,v2) in        
            var_map := (VarMap.add v1 {v1_node with sim = fl_v :: v1_node.sim} !var_map);
            let fl_v = (FSet.of_list fl,v1) in  
            var_map := (VarMap.add v2 {v2_node with sim = fl_v :: v2_node.sim} !var_map);
            ()
  |_ -> failwith " add_sim_to_node is only for Sim"

let add_fen_to_node atom = 
  match atom with
  | Fen (v1,fl) -> let v1_node = find_node v1 in  
          let fl = FSet.of_list fl in
          let fen_new = (if(FSet.is_empty v1_node.fen)then fl else (FSet.inter fl v1_node.fen)) in
          var_map := (VarMap.add v1 {v1_node with fen = fen_new} !var_map);
          ()
  |_ -> failwith "add_fen_to_node is only for Fen"

let rec list_remove ele = function
  |[] -> []
  |h::t when (h=ele) -> list_remove ele t 
  |h::t -> h::list_remove ele t

let z_eq_update vz v_l eq= 
  let vz_node = (find_node vz) in
  let vzeq = vz_node.equality in
  let rec helper ll vze =
    match ll with 
    |[]-> vze
    |(fl,v)::t -> if(List.mem v v_l) then
                  helper t (list_remove (fl,v) vze)
                  else helper t vze
  in 
  let n_eq =  (eq,List.hd v_l)::(helper vzeq vzeq) in
  var_map := (VarMap.add vz {vz_node with equality = n_eq} !var_map);
  ()

let z_sim_update vz v_l sim= 
  let vz_node = (find_node vz) in
  let vzeq = vz_node.sim in
  let rec helper ll vze =
    match ll with 
    |[]-> vze
    |(fl,v)::t -> if(List.mem v v_l) then
                  helper t (list_remove (fl,v) vze)
                  else helper t vze
  in 
  let n_sim =  (sim,List.hd v_l)::(helper vzeq vzeq) in
  var_map := (VarMap.add vz {vz_node with sim = n_sim} !var_map);
  ()

let eq_union eq1 eq2 = 
  let n_eq = ref eq1 in
  let rec helper1 l1 = 
      match l1 with
      | [] -> !n_eq
      | (l,v)::t -> let rec helper2 l2 =
                        match l2 with
                        |[]-> n_eq := (l,v)::!n_eq;
                                      ()
                        |(l2,v2)::t2 -> if(v=v2) then
                                        n_eq :=((FSet.union l2 l),v)::(list_remove (l2,v2) !n_eq) 
                                        else helper2 t2  (*BUG IMPLEMENT  z_eq_update vz vx vy eq*)
                    in
                    helper2 eq1;
                    helper1 t
  in helper1 eq2

let sim_union sim1 sim2 = 
  let n_sim = ref sim1 in
  let rec helper1 l1 = 
      match l1 with
      | [] -> !n_sim
      | (l,v)::t -> let rec helper2 l2 =
                        match l2 with
                        |[]-> n_sim := (l,v)::!n_sim;()
                        |(l2,v2)::t2 -> if(v=v2) then
                                        n_sim :=((FSet.inter l2 l),v)::(list_remove (l2,v2) !n_sim)
                                        else helper2 t2 (*BUG IMPLEMENT  z_sim_update vz vx vy eq*)
                    in
                    helper2 sim1;
                    helper1 t
  in helper1 sim2

let kind_union k1 k2 =
  match(k1,k2) with
  |(Unknown,Unknown)-> Unknown
  |(Unknown,k) -> k
  |(k,Unknown) -> k
  |(k,k3) when (k=k3) -> k
  | _ -> failwith "Kind miss match during union"

let node_union (n1:node) (n2:node):node = (*Do a clash check maybe*)
  let nf_var_l = VSet.union n1.var_l n2.var_l in
  let nf_equality = eq_union n1.equality n2.equality in
  let nf_sim = sim_union n1.sim n2.sim in 
  let nf_fen = FSet.inter n1.fen n2.fen in
  let nf_notfeat = n1.notfeat@n2.notfeat in
  let nf_id = "" in (*union occurs before we set the ids*)
  let nf_kind = kind_union n1.kind n2.kind in

  let merge _ n1 n2 = if(n1=n2) then Some n1 (*n1 and n2 are either the same value or are eqivalent*)
            else Some n1 (*Add a clash for if n1 and n2 are not equivalent*)
  in  
  let nf_feat = FMap.union merge n1.feat n2.feat in

  ({var_l = nf_var_l;feat = nf_feat;notfeat = nf_notfeat; equality = nf_equality;sim = nf_sim;fen=nf_fen; id = nf_id; kind = nf_kind})




let add_equal_to_node_ALL atom = 
  match atom with
  | Eq (v1,v2) -> let v1_node = find_node v1 in
          let v2_node = find_node v2 in
          let new_node = node_union v1_node v2_node in        
          var_map := VarMap.add v1 new_node !var_map;
          var_map := VarMap.add v2 new_node !var_map;
          ()

  |_ -> failwith "add_equal_to_node is only for Eq"




let invertRes (x:bool) (y:bool) = if(y)then (not x) else x
(*For equality f needs to exist in F and for sim no exist so use this*)
let contents z = 
   match z with
   |Some c -> c
   |None -> failwith "It was None"

let rec find_feat_link_opt (n:node) (f:feature) (vlist): var option=
  let var_l = VSet.elements n.var_l in
  let vlist = vlist@var_l in
  match (FMap.find_opt f n.feat) with
  | Some x -> add_feat_to_node (Feat((List.hd var_l),f,x)); 
              Some x 

  | None ->let rec helper1 l_l (flip:bool) : var option =
           match l_l with
           |[] -> None   
           |(l,v)::t when (not (List.mem v vlist)) -> 
                  let v_node = find_node v in
                  if (invertRes (FSet.mem f l) flip)then
                    begin
                      let x = (find_feat_link_opt v_node  f vlist) in 
                      if(x <> None)then
                        begin
                          let x = contents x in
                          add_feat_to_node (Feat((List.hd var_l),f,x)); (*Try Abs(v,f,x) if error *)
                          Some x 
                        end
                      else helper1 t flip
                    end
                  else helper1 t flip

            |_::t -> helper1 t flip
           in
         let x = helper1 n.equality false in (*Check indirectly trhough equality*)
         if(x = None) then (helper1 n.sim  true) (*Check indirectly trhough sim*)
         else x

(*Think about the clash condition where x =F y and x =G z and f belongs to both F,G but y[f] and g[f] are differnt*)

let disolve_node (n:node)= 
  let l = FSet.elements !fBigSet in
  let rec helper l =
    match l with
    |[] -> ()
    |f::t -> let _ = find_feat_link_opt n f [] in
             helper t
  in
  helper l 

let dissolve_all () =
  let var_map_l = VarMap.bindings !var_map in
  let rec helper var_map_l  = 
    match var_map_l with 
    |[] -> ()
    |(_,n_1)::t -> disolve_node n_1 ;
                     helper t
  in 
  helper var_map_l 

let not_Fen_transform atom  =
  match atom with
  |Fen(v1,fl) ->  let v1_node = find_node v1 in
                  let all_f = FSet.elements (FSet.diff !fBigSet (FSet.of_list fl)) in 
                  let helper2 () = 
                        if(FSet.is_empty v1_node.fen) then
                          begin
                            let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                            let v_new = (VarMap.cardinal !var_map) + 3 in
                            var_map := VarMap.add v_new (empty_node v_new) !var_map;
                            add_feat_to_node (Feat (v1,new_f,v_new));
                            fBigSet := FSet.add new_f !fBigSet;
                            ()
                          end
                        else failwith "not_Fen_transform: [Clash]It is not possible to satisfy it"
                  in
                  let rec helper l  =
                    match l with
                    |[] -> helper2 ()
                    |f::t -> let x = find_feat_link_opt v1_node f [] in
                             if(x = None) then helper t 
                             else ()
                  in
                  helper all_f
  |_ -> failwith "not_Fen_transform is only for Fen"

let is_allowed (atom):bool =
  match atom with 
  |Feat(v1,f,v2)-> not (List.mem (f,v2) (find_node v1).notfeat)
  |Abs(v1,f) ->  not (List.mem (f,0) (find_node v1).notfeat)
  |_ -> failwith "is_allowed: is only for Feat and Abs"


            
let not_eq_sim_transform  atom =
  let getValF  = match atom with 
               |Eqf(v1,fl,v2) -> (v1,fl,v2,false)
               |Sim(v1,fl,v2) -> let fl = FSet.elements(FSet.diff !fBigSet (FSet.of_list fl)) in
                                (v1,fl,v2,true)
               |_ -> failwith "node_not_eq_sim_transform is only for Eqf and Sim"
  in
  let (v1,all_f,v2,isSim) = getValF in
  let v1_node = find_node v1 in
  let v2_node = find_node v2 in
  let info = ref [] in
  let helper4 () = if((FSet.is_empty v1_node.fen)&&(isSim))then  
                  begin
                    let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                    let v_new_1 = (VarMap.cardinal (!var_map)) + 3 in
                    var_map := VarMap.add v_new_1 (empty_node v_new_1) (!var_map);
                    add_feat_to_node (Feat (v1,new_f,v_new_1));
                    add_abs_to_node (Abs (v2,new_f));
                    fBigSet := FSet.add new_f !fBigSet;
                    ()
                  end
                else if((FSet.is_empty v2_node.fen)&&(isSim)) then
                  begin
                    let (new_f:feature) = "GeneratedF"^ string_of_int (fresh ()) in
                    let v_new_1 = (VarMap.cardinal (!var_map)) + 3 in
                    var_map := VarMap.add v_new_1 (empty_node v_new_1) (!var_map);
                    add_feat_to_node (Feat (v2,new_f,v_new_1));
                    add_abs_to_node (Abs (v1,new_f));
                    fBigSet := FSet.add new_f !fBigSet;
                    ()
                  end
                else failwith "node_not_eq_transform : Could not find a way to satisfy it"
    in
  let rec helper3 info_s=
        match info_s with 
        |[] -> helper4 () (*For sim we can still add a new feature*)
        |(f1,None,None)::t -> if (((FSet.is_empty v1_node.fen)||(FSet.mem f1 v1_node.fen))&&(is_allowed (Abs (v2,f1)) )) then 
                              let v_new = (VarMap.cardinal (!var_map)) + 3 in
                              var_map := VarMap.add v_new (empty_node v_new) (!var_map);
                              add_feat_to_node (Feat (v1,f1,v_new));
                              add_abs_to_node (Abs (v2,f1));
                              ()
                           else if(((FSet.is_empty v2_node.fen)||(FSet.mem f1 v2_node.fen))&&(is_allowed (Abs (v1,f1)) )) then 
                              let v_new = (VarMap.cardinal (!var_map)) + 3 in
                              var_map := VarMap.add v_new (empty_node v_new) (!var_map);
                              add_feat_to_node (Feat (v2,f1,v_new));
                              add_abs_to_node (Abs (v1,f1));
                              ()
                           else helper3 t
        |_::t -> helper3 t
        in
  let rec helper2 info_s =
        match info_s with 
        |[] -> helper3 !info
        |(f1,Some _, None)::t ->  if(is_allowed (Abs (v2,f1))) then helper2 t
                                    else add_abs_to_node (Abs (v2,f1))
                                    
        |(f1,None, Some _)::t ->  if(is_allowed (Abs (v1,f1))) then helper2 t
                                    else add_abs_to_node (Abs (v1,f1)) 
                                    
        |_::t -> helper2 t

        in 
  let rec helper1 all_f  = 
      match all_f with 
      |[]-> helper2 !info
      |f::t -> let x = (find_feat_link_opt v1_node f [] ) in
               let y = (find_feat_link_opt v2_node f [] ) in
               info := (f,x,y)::!info;
               match !info with
               |[]-> failwith "Not a possible match" 
               |(_,Some v_1,Some v_2)::_ -> if(v_1=v_2) then helper1 t
                                            else ()
               |_::_ -> helper1 t  
      in
    helper1 all_f 
                    
(*I-> Eqf,Sim,Fen
  II-> Eq and union
  III-> Feat and Abs
  IV -> Not Feat and Not abs
  V -> Not (Fen,Eq,Eqf,Sim)
  Dissolve*)

let rec clause_phase_I (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Eqf(v1,fl,v2)::t -> add_equal_to_node (Eqf(v1,fl,v2));
                         clause_phase_I t
    |Pos Sim(v1,fl,v2)::t -> add_sim_to_node (Sim(v1,fl,v2));
                         clause_phase_I t
    |Pos Fen(v1,fl)::t -> add_fen_to_node (Fen(v1,fl));
                      clause_phase_I t
    |Pos Kind(v1,k)::t -> add_kind_to_node (Kind(v1,k));
                      clause_phase_I t
    |Neg Kind(v1,k)::t -> let k  = if(k = Dir) then Reg else if(k= Reg) then Dir else Other in
                          add_kind_to_node (Kind(v1,k));  
                      clause_phase_I t
    | _ :: t -> clause_phase_I t

let rec clause_phase_II (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Eq(v1,v2)::t -> let new_node = node_union (find_node v1) (find_node v2) in
                     var_map := VarMap.add v1 new_node !var_map;
                     var_map := VarMap.add v2 new_node !var_map;
                     clause_phase_II t
    | _ :: t -> clause_phase_II t

let rec clause_phase_III (clau:clause) =
    match clau with 
    |[] -> ()
    |Pos Feat(v1,f,v2)::t -> add_feat_to_node (Feat(v1,f,v2));
                      clause_phase_III t
    |Pos Abs (v1,f)::t -> add_abs_to_node (Abs(v1,f));
                      clause_phase_III t
    | _ :: t -> clause_phase_III t

let rec clause_phase_IV (clau:clause) =
    match clau with 
    |[] -> ()
    |Neg Feat(v1,f,v2)::t -> no_feat_abs_to_node (Feat(v1,f,v2));
                      clause_phase_IV t
    |Neg Abs (v1,f)::t -> no_feat_abs_to_node (Abs(v1,f));
                      clause_phase_IV t
    | _ :: t -> clause_phase_IV t

let rec clause_phase_V (clau:clause) =
    match clau with 
    |[] -> ()
    |Neg Fen(v1,fl)::t -> not_Fen_transform (Fen(v1,fl));
                      clause_phase_V t
    |Neg Eqf(v1,fl,v2)::t -> if(v1=v2) then failwith "Clash: x =/=F x" 
                             else not_eq_sim_transform (Eqf(v1,fl,v2));
                                 clause_phase_V t
    |Neg Eq(v1,v2)::t -> if(v1=v2) then failwith "Clash: x =/= x" 
                         else not_eq_sim_transform (Eqf(v1,FSet.elements !fBigSet,v2));
                              clause_phase_V t
    |Neg Sim(v1,fl,v2)::t -> if(v1=v2) then failwith "Clash: x ~/~F x" 
                             else not_eq_sim_transform (Sim(v1,fl,v2));
                             clause_phase_V t
    | _ :: t -> clause_phase_V t


(*EXAMPLE-TEST*)

let v1:var = fresh ()
let v2:var = fresh ()
let v3:var = fresh ()
let v4:var = fresh ()
let v5:var = fresh ()
let v6:var = fresh ()
let v7:var = fresh ()
let v8:var = fresh ()
let v9:var = fresh ()
let v10:var = fresh ()
let v11:var = fresh ()
let v12:var = fresh ()

let f1:feature = "lib"
let f2:feature = "share"
let f3:feature = "bin"
let f4:feature = "usr"
let f5:feature = "racid"
let f6:feature = "apache.conf"
let f7:feature = "lg.conf"
let f8:feature = "etc"


let (clau_1:clause) = [ Pos (Feat(v1,"a",v2));Pos (Feat(v1,"c",v3));
          Pos (Feat(v1,"d",v4));Pos (Feat(v5,"a",v6));Pos (Feat(v5,"c",v7));
          Pos (Feat(v5,"d",v8));Pos (Feat(v2,"b",v9)); Pos (Eq(v4,v8));
          Pos (Eq(v2,v7));Pos (Abs(v1,"abc"));Pos (Abs(v5,"abc"));Pos (Kind(v9,Reg))]

(*  [Feat (1, "lib", 2); Feat (1, "share", 3); Feat (4, "bin", 6);
   Feat (4, "usr", 7); Eqf (1, ["lib"; "share"], 7); Feat (8, "etc", 9);
   Eqf (8, ["usr"], 4); Feat (9, "racid", 10); Feat (10, "apache.conf", 11);
   Feat (10, "lg.conf", 12)]
*)


let get_vBigSet () =
    let ll = VarMap.bindings !var_map in
    let rec helper ll =
      match ll with
      |[] -> []
      |(v,_)::t -> v::(helper t)
    in helper ll

let get_unreachable () =
  let vBigSet = ref (get_vBigSet ())in
  let ll = VarMap.bindings !var_map in
  let rec helper1 ll =
    match ll with
    |[] -> (list_remove 0 !vBigSet)
    |(_,v_node)::t -> let l = FMap.bindings (v_node.feat) in
                      let rec helper2 l =
                        match l with
                        |[] -> helper1 t
                        |(_,v2)::t2-> vBigSet := (list_remove v2 !vBigSet);
                                   helper2 t2
                      in helper2 l
  in helper1 ll

let rec get_path (v) (v_cycle) (path) (f)=
  let ll = FMap.bindings ((find_node v).feat) in
  if(List.mem v v_cycle)then failwith "Cycle Clash"
  else if((ll=[])||(v=0)) then
      paths := (path,f,v)::(!paths)
  else
    (let rec helper ll =
      match ll with
      |[] -> ()
      |(f2,v2)::t when v2 = 0 -> 
                     get_path (v2) (v::v_cycle) (path) (f2);
                     helper t
      |(f2,v2)::t -> if((find_node v2).kind = Reg) then
                        (get_path (v2) (v::v_cycle) (path) (f2);
                        helper t)
                     else   
                        (get_path (v2) (v::v_cycle) (path^"/"^f2) ("");
                        helper t)
    in helper ll)

let rec mkdir_from_path path_list =
  match path_list with 
  |[] -> ()
  |(h,f,v)::t when ((v <> 0) && (f <> "")) -> 
           let h1 = "mkdir -p "^h in
           let h2 = "touch "^(h^"/"^f) in
           Format.printf "%s\n %s\n" h1 h2 ;ignore (Sys.command h1);
           ignore (Sys.command h2); 
           mkdir_from_path t

  |(h,_,_)::t -> let h = "mkdir -p "^h in
           Format.printf "%s\n" h ;ignore (Sys.command h); 
           mkdir_from_path t

let rec check_path path_list =
  match path_list with 
  |[] -> true
  |(h,f,_)::t when f = "" -> 
                Format.printf "check : %s\n" h ;
                if(Sys.file_exists h)then check_path t else false
  |(h,f,v)::t when v = 0->
                let h2 = (h^"/"^f) in
                Format.printf "check : %s\t" h ;
                Format.printf "check Abs : %s\n" h2 ;
                if((Sys.file_exists h) && (not (Sys.file_exists h2)))then 
                check_path t else false
  |(h,f,_)::t -> Format.printf "check Reg : %s\n" (h^"/"^f) ;
              if(Sys.file_exists h)then check_path t else false



let shell_script () =
  ignore (Sys.command "mv ./a/b ./c");()

let safe_dir =  "/media/ap/New Volume/IIIT Kalyani/Internships/Feature Tree Logic/Reverse/ADifferentWay/Test region/InnerTR/Inner2TR/Inner3TR"
let create_TR () =
  ignore (Sys.chdir safe_dir);
  ignore (Sys.command "mkdir ./TR");
  Sys.chdir("./TR");()

let clean_TR () = 
  Sys.chdir("..");
  ignore (Sys.command "rm -r ./TR/*");
  Sys.chdir("./TR");()


(*make it boolean return for engine.ml and take the roots as input*)
let test_files ()=
  let l = get_unreachable () in
  let rec helper l count = 
    match l with 
    |[root_after;root_before] -> 
                      if(count = 3) then failwith "Test Fail"
                      else
                        create_TR ();
                        clean_TR ();
                        paths:= [];
                        get_path root_before [] "." "";
                        mkdir_from_path (!paths);
                        shell_script ();
                        paths:= [];
                        get_path root_after [] "." ""; 
                        if(check_path (!paths)) then Format.printf "CHECK SUCCESS"
                        else 
                        (Format.printf "Failure\n" ;(helper [root_before;root_after] (count+1)))
                     
    |_ -> failwith "Not exactly 2 unreachable"
  in helper l 1


let engine (clau_1:clause) =
  var_map := VarMap.empty;
  fBigSet := FSet.empty;
  create_empty_var_map clau_1; 
  clause_phase_I clau_1;
  clause_phase_II clau_1;
  clause_phase_III clau_1;
  clause_phase_IV clau_1;
  clause_phase_V clau_1;
  dissolve_all ();
  var_map_display !var_map;
  (*execute !mkdir*)




