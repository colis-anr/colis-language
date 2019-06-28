open SymbolicUtility

module IdMap = Env.IdMap

let register () =
  List.iter register [
    (module Basics.True) ;
    (module Basics.False) ;
    (module Basics.Echo) ;
    (module Dpkg) ;
    (module DpkgMaintscriptHelper) ;
    (module Mv);
    (module Mkdir);
    (module Rm) ;
    (module Test) ;
    (module Test.Bracket) ;
    (module Touch) ;
    (module UpdateAlternatives) ;
    (module Which) ;
    (module Which.Silent) ;
  ]
