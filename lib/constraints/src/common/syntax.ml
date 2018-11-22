type atom =
  | Eq of Var.t * Var.t
  | Feat of Var.t * Feat.t * Var.t
  | Abs of Var.t * Feat.t
  | Reg of Var.t
  | Dir of Var.t
  | Fen of Var.t * Feat.Set.t
  | Sim of Var.t * Feat.Set.t * Var.t

type literal =
  | Pos of atom
  | Neg of atom

type formula =
  | Atom of atom
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Exists of Var.t * formula
  | Forall of Var.t * formula
