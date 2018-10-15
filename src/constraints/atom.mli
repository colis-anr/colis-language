
type t =
  | Eq of Variable.t * Variable.t
  | Feat of Variable.t * Feature.t * Variable.t
  | Abs of Variable.t * Feature.t
  | Fen of Variable.t * Feature.Set.t
  | Sim of Variable.t * Feature.Set.t * Variable.t

