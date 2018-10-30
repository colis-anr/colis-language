include Constraints_common

module Implementation = Constraints_implementation

module EfficientClause = Interface.Make(Implementation.Efficient)
module Clause = EfficientClause
