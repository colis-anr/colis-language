(*This section is under development, Thus some parts might not function properly*)
(*What if we think of a node as Absent and map feature to its var when ever Ab(x,f)*)
(*CHECK IMPORTANT COMMENT*)
(*VARIOUS TYPES NEEDED*)
type feature = string
let compare = compare
let equal f1 f2 = compare f1 f2 = 0

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


type node = { var_l: var list;
			 feat: var FMap.t;
			 abFeat: FSet.t;
			 equality: (FSet.t*var) list; (*Maybe a set will be more efiecient*)
			 }

let empty_node ():node = {var_l = [];feat = FMap.empty;abFeat = FSet.empty;equality = []}
let empty_node v:node = {var_l = [v];feat = FMap.empty;abFeat = FSet.empty;equality = []}

type atom =
  | Eq of var * var
  | Eqf of var * feature list * var
  | Feat of var * feature * var
  | Abs of var * feature
  | Fen of var * feature list
  | Sim of var * feature list * var

type var_map_type = node VarMap.t

type clause = atom list





(*VARIOUS FUNCTIONS NEEDED*)


let node_display {feat = feat_ ;abFeat = abFeat_ ;equality= equality_} : unit = 
		
		let feat_ = FMap.bindings feat_ in
		let rec feat_display feat_ = 
			match feat_ with 
			|[] -> ()
			|(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" f_1 v_1;
											 feat_display t 
		in 
		
		let abFeat_ = FSet.elements abFeat_ in
		let rec abFeat_display abFeat_ = 
			match abFeat_ with 
			|[] -> ()
			| h::t -> Format.printf "[%s]\t" h;
								abFeat_display t
		in 
		let rec equality_display equality_ = 
			match equality_ with 
			|[] -> ()
			|(f_1,v_1)::t -> Format.printf "[%s --> %d]\t" "F" v_1; (*ADD FOR FSET*)
								equality_display t
		in

		Format.printf "Features:\n";
		feat_display feat_;
		Format.printf "\nAbsent Features:\n";
		abFeat_display abFeat_;
		Format.printf "\nEquality:\n";
		equality_display equality_

let var_map_display var_map = 
		let var_map = VarMap.bindings var_map in
		let rec helper var_map = 
			match var_map with 
			|[] -> ()
			|(v_1,n_1)::t -> Format.printf "\n\t\tNODE(VAR) : %d\n" v_1;
										node_display n_1;
										helper t
		in 
		helper var_map




let rec create_empty_var_map clause var_map = 
	match clause with 
	|[] -> var_map
	|Feat(v1,f,v2)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
											 let var_map = VarMap.add v2 (empty_node v2) var_map in
											 create_empty_var_map t var_map

	|Abs (v1,f)::t -> let var_map = VarMap.add v1 (empty_node v1) var_map in
										create_empty_var_map t var_map

	|Eqf (v1,fl,v2)::t ->let var_map = VarMap.add v1 (empty_node v1) var_map in
										let var_map = VarMap.add v2 (empty_node v2) var_map in
										create_empty_var_map t var_map
	|_::t -> failwith "create_empty_var_map"

(*Creates an empty node if not found*)
let find_node_c var_map v1 = 
	let a = VarMap.find_opt v1 var_map in
	match a with
	| None -> let emt_n = empty_node v1 in
			  let var_map = VarMap.add v1 emt_n var_map in
			  (emt_n,var_map)
	| Some nod -> (nod,var_map)

let find_node var_map v1 = 
	let a = VarMap.find_opt v1 var_map in
	match a with
	| None -> failwith "Could not find Var"
	| Some nod -> nod 

let add_feat_to_node atom var_map = 
	match atom with
	| Feat(v1,f,v2) -> let v1_node = find_node var_map v1 in
					   (VarMap.add v1 {v1_node with feat = FMap.add f v2 v1_node.feat} var_map)
	|_ -> failwith "add_feat_to_node is only for Feat"


let add_abs_to_node atom var_map = 
	match atom with
	| Abs (v1,f) -> let v1_node = find_node var_map v1 in				
					(VarMap.add v1 {v1_node with abFeat = FSet.add f v1_node.abFeat} var_map)
	|_ -> failwith "add_abs_to_node is only for Abs"

let add_equal_to_node atom var_map = 
	match atom with
	| Eqf (v1,fl,v2) -> let v1_node = find_node var_map v1 in
											let v2_node = find_node var_map v2 in
						let fl_v = (FSet.of_list fl,v2) in 				
						let var_map = (VarMap.add v1 {v1_node with equality = fl_v :: v1_node.equality} var_map) in
						let fl_v = (FSet.of_list fl,v1) in 	
						(VarMap.add v2 {v2_node with equality = fl_v :: v2_node.equality} var_map)
	|_ -> failwith "add_equal_to_node is only for Eqf"


let node_union (n1:node) (n2:node):node= 
	let nf_abFeat = FSet.union n1.abFeat n2.abFeat in
	let nf_equality = n1.equality @ n2.equality in
	let nf_var_l = n1.var_l @ n2.var_l in
	let merge k n1 n2 = if(n1=n2) then Some n1
						else failwith "Crash:feat_map_combine_ALL";in
	
	let nf_feat = FMap.union merge n1.feat n2.feat in

	({var_l = nf_var_l;feat = nf_feat; abFeat = nf_abFeat; equality = nf_equality})
	


let add_equal_to_node_ALL atom var_map = 
	match atom with
	| Eq (v1,v2) -> let v1_node = find_node var_map v1 in
					let v2_node = find_node var_map v2 in
					let new_node = node_union v1_node v2_node in 				
					let var_map = VarMap.add v1 new_node var_map in
					VarMap.add v2 new_node var_map

	|_ -> failwith "add_equal_to_node is only for Eq"

let rec clause_feat_abs (clau:clause) var_map =
		match clau with 
		|[] -> var_map
		|Feat(v1,f,v2)::t -> clause_feat_abs t (add_feat_to_node (Feat(v1,f,v2)) var_map)
		|Abs (v1,f)::t -> clause_feat_abs t (add_abs_to_node (Abs(v1,f)) var_map)
		| _ :: t -> clause_feat_abs t var_map


let rec clause_eqf_eq (clau:clause) var_map =
		match clau with 
		|[] -> var_map
		|Eqf(v1,fl,v2)::t -> clause_eqf_eq t (add_equal_to_node (Eqf(v1,fl,v2)) var_map)
		| _ :: t -> clause_eqf_eq t var_map




let rec find_feat_link_opt (n:node) (var_map) (f:feature) (vlist)=
	let vlist = vlist@n.var_l in
	match (FMap.find_opt f n.feat) with
	| Some x -> let n = {n with feat = FMap.add f x n.feat} in
				let rec helper3 var_map vl =
					match vl with 
					|[]-> var_map 
					|v1::t -> helper3 (VarMap.add v1 n var_map) t
				in
				((helper3 var_map n.var_l),x) 

	| None -> let rec helper1 l_l var_map =
			  	 match l_l with
			  	 |[] -> (var_map,0)   (*Returns 0 when not find change it with a option*) 
			  	 |(l,v)::t when (not (List.mem v vlist)) -> let l = FSet.elements l in
			  	 			  let v_node = find_node var_map v in
			  	 			  let rec helper2 l var_map = 
			  	 				 match l with 
			  	 				 |[]-> helper1 t var_map
			  	 				 |h2::t2 -> if(h2 = f) then
			  	 				 								let (var_map,x) = (find_feat_link_opt v_node var_map f vlist) in (* IMPORTANT ADD if for x = 0 in that case don't update(as 0 means absent) and continue helper 1 (maybe)*)
			  	 				 								let v_node = {v_node with feat = FMap.add f x v_node.feat} in
																	let rec helper3 var_map vl =
																		match vl with 
																		|[]-> var_map 
																		|v1::t -> helper3 (VarMap.add v1 v_node var_map) t
																	in
																	((helper3 var_map n.var_l),x) 

			  	 				 						else helper2 t2 var_map
			  	 			   in 
			  	 			   helper2 l var_map
			  	 	|_::t -> helper1 t var_map
			   in helper1 n.equality var_map


(*Think about the clash condition where x =F y and x =G z and f belongs to both F,G but y[f] and g[f] are differnt*)
(*Think about abs*)
let rec disolve_node (n:node) var_map = 
	let rec helper1 l_l var_map = 
		match l_l with 
		|[] -> var_map 
		|(l,v)::t -> let l = FSet.elements l in
								let rec helper2 l var_map= 
										match l with 
										|[] -> helper1 t var_map
										|h2::t2 -> let (var_map,x) = find_feat_link_opt n var_map h2 [] in 
															 helper2 t2 var_map
								in 
								(helper2 l var_map)
	in 
	(helper1 n.equality var_map)

let dissolve_all var_map =
	let var_map_l = VarMap.bindings var_map in
	let rec helper var_map_l var_map = 
		match var_map_l with 
		|[] -> var_map
		|(v_1,n_1)::t -> let var_map = disolve_node n_1 var_map in
									   helper t var_map
	in 
	helper var_map_l var_map



















(*EXAMPLE-TEST*)

let v1:var = 1
let v2:var = 2
let v3:var = 3
let v4:var = 4
let v5:var = 5
let v6:var = 6
let v7:var = 7
let v8:var = 8
let v9:var = 9
let v10:var = 10
let v11:var = 11
let v12:var = 12

let f1:feature = "lib"
let f2:feature = "share"
let f3:feature = "bin"
let f4:feature = "usr"
let f5:feature = "racid"
let f6:feature = "apache.conf"
let f7:feature = "lg.conf"
let f8:feature = "etc"


let (clau_1:clause) = [Feat(v1,f1,v2);Feat(v1,f2,v3);Feat(v4,f3,v6);Feat(v4,f4,v7);Eqf(v1,[f1;f2],v7);
					Feat(v8,f8,v9);Eqf(v8,[f4],v4);Feat(v9,f5,v10);Feat(v10,f6,v11);Feat(v10,f7,v12)]

(*  [Feat (1, "lib", 2); Feat (1, "share", 3); Feat (4, "bin", 6);
   Feat (4, "usr", 7); Eqf (1, ["lib"; "share"], 7); Feat (8, "etc", 9);
   Eqf (8, ["usr"], 4); Feat (9, "racid", 10); Feat (10, "apache.conf", 11);
   Feat (10, "lg.conf", 12)]
*)
let (var_map:var_map_type) = create_empty_var_map clau_1 VarMap.empty


let var_map1 = clause_feat_abs clau_1 var_map
let var_map2 = clause_eqf_eq clau_1 var_map1
let var_map3 = dissolve_all var_map2
let () = var_map_display var_map

(*
#trace dissolve_all;;
#trace disolve_node;;
#trace find_feat_link_opt;;
*)
