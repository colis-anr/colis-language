include Constraints_common

module Implementation = Constraints_implementation
module Interface = Interface

module NaiveClause = Interface.Make(Implementation.Naive)
module EfficientClause = Interface.Make(Implementation.Efficient)

module Clause = EfficientClause
