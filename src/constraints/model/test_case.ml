(*Base Clause-PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eqf(v8,[f4],v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12)))]

(*Equality-PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1))]

(*Equality with union of eqf - PASS (Test other paths)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Eqf(v8,[f1],v2)),Pos (Eqf,(v4,[f3],v2))]

(*Equality with  intersection of eqf sim - PASS (Test other paths)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Sim(v8,[f1;f4],v2));Pos (Sim(v4,[f1;f3],v2))]

(*Continue from here*)


(*Equality with Fen*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Fen(v8,[f3,f4,f8,f5])),Pos (Fen(v4,[f3,f4,f8,f1,f6]))]


(*Absent*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1))]

(*Fen-Clash*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1]))]

(*Fen*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1,f2,f3]))]

(*Sim*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Sim(v1,[f4;f7],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1,f2,f3]))]

(*Not Feature+Equality*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Feat(v8,f1,v3))]

(*Not Feature - indirect clash*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Feat(v8,f3,v6))]

(*Not Abs+Equality*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Abs(v8,f1))]

(*Not Feature - indirect clash*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v4,f1));Neg (Abs(v8,f1))]