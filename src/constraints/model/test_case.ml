(*1 Base Clause-PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eqf(v8,[f4],v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12)))]

(*2 Equality-PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1))]

(*3 Equality with union of eqf - PASS (Test other paths)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Eqf(v8,[f1],v2)),Pos (Eqf,(v4,[f3],v2))]

(*4 Equality with  intersection of eqf sim - PASS (Test other paths)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Sim(v8,[f1;f4],v2));Pos (Sim(v4,[f1;f3],v2))]

(*5 Equality with Fen - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Fen(v8,[f3;f4;f8;f5]));Pos (Fen(v4,[f3;f4;f8;f1;f6]))]


(*6 Absent - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1))]

(*7 Fen-Clash - PASS(Fen clash)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1]))]

(*8 Fen - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1,f2,f3]))]

(*9 Sim - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Sim(v1,[f4;f7],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Pos(Fen(v1,[f1;f2;f3]))]

(*10 Neg Feature + Pos Equality - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Feat(v8,f1,v3))]

(*11 Neg Feature + Pos Equality - indirect clash - PASS(not feature clash)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Feat(v8,f3,v6))]

(*12 Neg Abs + Pos Equality - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Neg (Abs(v8,f1))]

(*13 Neg Abs+ Pos Equality - indirect clash - PASS(not feature clash)*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v4,f1));Neg (Abs(v8,f1))]

(*14 Neg Fen - Already Satisfied - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Abs(v10,f5));Pos (Abs(v8,f1));Neg (Fen(v1,[f1;f3]))]

(*15 Neg Eqf - Already Satisfied - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Feat(v1,f4,v5));Pos (Feat(v7,f4,v2)); Neg(Eqf(v1,[f1;f2;f4],v7))]

(*16 Neg Eqf - Add an absent - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Feat(v1,f4,v5)); Neg(Eqf(v1,[f1;f2;f4],v7))]

(*17 Neg Eqf - Add an absent and a mapping - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12)); Neg(Eqf(v1,[f1;f2;f4],v7))]

(*18 Neg Sim - Already Satisfied - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Feat(v1,f3,v5));Pos (Feat(v7,f3,v2)); Neg(Sim(v1,[f1;f2;f4],v7))]

(*19 Neg Sim - Add an absent - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12));Pos (Feat(v7,f3,v2)); Neg(Sim(v1,[f1;f2;f4],v7))]

(*20 Neg Sim - Add an absent and a mapping - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12)); Neg(Sim(v1,[f1;f2;f4],v7))]

(*20 Neg Sim - Add a new feature for absent and mapping - PASS*)
let (clau_1:clause) = [ Pos (Feat(v1,f1,v2));Pos (Feat(v1,f2,v3));
          Pos (Feat(v4,f3,v6));Pos (Feat(v4,f4,v7));Pos (Eqf(v1,[f1;f2],v7));
          Pos (Feat(v8,f8,v9));Pos (Eq(v8,v4));Pos (Feat(v9,f5,v10));
          Pos (Feat(v10,f6,v11));Pos (Feat(v10,f7,v12)); Neg(Sim(v1,[f3;f4;f5;f6;f7;f8],v7))]
