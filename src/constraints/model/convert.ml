open Common

let feat_to_string (x:Colis_constraints_common.Feat.t):string = (Colis_constraints_common.Feat.to_string x) 

let var_to_int (x:Colis_constraints_common.Var.t):int =
  let rec helper in_s out_s=
    if((String.length in_s) < 3) then
      int_of_string out_s
    else
    let ch = String.sub in_s 0 3 in
    let digit =
      (match ch with
       | "₀" -> "0"
       | "₁" -> "1"
       | "₂" -> "2"
       | "₃" -> "3"
       | "₄" -> "4"
       | "₅" -> "5"
       | "₆" -> "6"
       | "₇" -> "7"
       | "₈" -> "8"
       | "₉" -> "9"
       | _ -> assert false) in
    let in_s = String.sub in_s 3 ((String.length in_s) - 3) in
    helper in_s (out_s^digit)
  in 
  (helper (Colis_constraints_common.Var.to_string x) "")

let fset_to_fset (x:Colis_constraints_common.Feat.Set.t): string list = 
  let lis = Colis_constraints_common.Feat.Set.elements x in
  let rec helper = function
   |[]-> []
   |h::t -> (feat_to_string h)::helper t
  in (helper lis)

let kind_to_kind (x:Colis_constraints_common.Kind.t): Common.kindt =
  match x with
  | Dir -> Dir
  | Reg -> Reg
  | Char | Sock | Pipe | Symlink | Block -> Other

let atom_to_Atom (x: Colis_constraints_common.Atom.t): Common.atom =
  match x with
  | Eq(v1,v2) -> Eq(var_to_int v1,var_to_int v2)
  | Feat(v1,f,v2) -> Feat (var_to_int v1,feat_to_string f,var_to_int v2)
  | Abs(v1,f) -> Abs(var_to_int v1,feat_to_string f)
  | Maybe (v1,f,v2) -> Maybe (var_to_int v1,feat_to_string f,var_to_int v2)
  | Kind(v1,k) -> Kind(var_to_int v1,(kind_to_kind k))
  | Fen(v1,f) -> Fen(var_to_int v1,fset_to_fset f)
  | Sim(v1,f,v2) -> Sim(var_to_int v1,fset_to_fset f,var_to_int v2)

let rec clause_to_clause (x: Colis_constraints_common.Literal.t list): Common.literal list =
  match x with
  | [] -> []
  | Pos a::t -> Pos (atom_to_Atom a):: clause_to_clause t
  | Neg a::t -> Neg (atom_to_Atom a):: clause_to_clause t
 
