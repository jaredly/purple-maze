open Shared;
open Reprocessing;

let speed = 1.1;
let keys = [
  (Events.Left, (-. speed, 0.)),
  (Events.Right, (speed, 0.)),
  (Events.Up, (0., -. speed)),
  (Events.Down, (0., speed)),
];

let maxSpeed = 3.;

let lineRect = ((a, b), (c, d), stroke) => {
  let hs = stroke /. 2.;
  if (a <= c && b <= d) {
    ((a -. hs, b -. hs), (max(c -. a, stroke)), max(d -. b, stroke))
  } else {
    ((c -. hs, d -. hs), (max(a -. c, stroke)), max(b -. d, stroke))
  }
};

let collide = (x, y, dx, dy, walls) => {
  List.fold_left(
    ((dx, dy), wall) => switch wall {
    | Mazer.Border.Arc(_) => assert(false)
    | Line((p1, p2)) => {
      let (pos, w, h) = lineRect(p1, p2, 6.);
      let intersect = Reprocessing.Utils.intersectRectCircle(~rectPos=pos, ~rectH=h, ~rectW=w, ~circleRad=10.);
      if (intersect(~circlePos=(x +. dx, y +. dy))) {
        if (intersect(~circlePos=(x, y +. dy))) {
          (dx, 0.)
        } else if (intersect(~circlePos=(x +. dx, y))) {
          (0., dy)
        } else {
          (0., 0.)
        }
      } else {
        (dx, dy)
      }
    }
    },
    (dx, dy),
    walls
  )
};

let step = ({player, walls} as state, env) => {

  let (dx, dy) = List.fold_left(
    ((dx, dy), (k, (ax, ay))) => {
      if (Env.key(k, env)) {
        (dx +. ax, dy +. ay)
      } else {
        (dx, dy)
      }
    },
    (player.dx, player.dy),
    keys
  );

  let dx = max(min(maxSpeed, dx), -. maxSpeed) *. 0.87;
  let dy = max(min(maxSpeed, dy), -. maxSpeed) *. 0.87;
  let (dx, dy) = collide(player.x, player.y, dx, dy, walls);
  let x = player.x +. dx;
  let y = player.y +. dy;

  {...state, player: {x, y, dx, dy}}
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
  let player = Man.randomCoord(state);
  (walls, player, Man.randomCoord(state));
};

let newGame = state => state;