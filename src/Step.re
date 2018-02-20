open Shared;
open Reprocessing;

let step = (state, env) => {
  state
  /* let distance = getDistance(state.status);
  switch (state.status) {
  | Playing(distance) => {
    gameStep(distance, state, env)
  }
  | _ => state
  } */
};

let makeMaze = () => {
  let module Board = Mazer.NewRect;
  let module Alg = Mazer.NewDepth.F(Mazer.NewDepth.RandomConfig({}));

  let module Man = Mazer.Manager.F(Board, Alg);

  let (width, height) = (500., 500.);
  let min_margin = 10.;
  let size_hint = 10;

  let with_margins = (width -. min_margin *. 2.0, height -. min_margin *. 2.0);
  let state = Man.init(with_margins, size_hint);
  let state = Man.loop_to_end(state);

  let walls = Man.all_walls(state);
  walls
};

let newGame = state => state;