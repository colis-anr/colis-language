include Constraints_common

module Implementation = Constraints_implementation

module DummyClause = Interface.Make(Implementation.Dummy)
module NaiveClause = Interface.Make(Implementation.Naive)
module EfficientClause = Interface.Make(Implementation.Efficient)

module Clause = EfficientClause
