let module Hot = Mazere.NewRect;
let module Board = Mazere.NewRect;
/* let module Board = Mazere.TriangleBoard; */
let module Alg = Mazere.NewDepth.F(Mazere.NewDepth.RandomConfig({}));
let module Man = Mazere.Manager.F(Board, Alg);

Reprocessing.hotreload("src/dev.re");
