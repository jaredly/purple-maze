open Shared;
open Reprocessing;

let makeMaze = (curPos) => {
  /* Not so much */
  /* let module Board = Mazere.HexBox; */
  /* let module Board = Mazere.NewHexTriangle; */
  /* Good */
  /* let module Board = Mazere.NewRect; */
  let module Board = Mazere.TriangleBoard;
  /* let module Board = Mazere.SquareTriangle; */
  /* let module Board = Mazere.Circle; */
  let module Alg = Mazere.NewDepth.F(Mazere.NewDepth.RandomConfig({}));

  let module Man = Mazere.Manager.F(Board, Alg);

  Man.randInit();
  let (width, height) = (800., 800.);
  let min_margin = 10.;
  let size_hint = 15;

  let with_margins = (width -. min_margin *. 2.0, height -. min_margin *. 2.0);
  let state = Man.init(with_margins, size_hint);
  let state = Man.loop_to_end(state);

  let walls = Man.all_walls(state);
  /* let items = ref([]);
  for (i in 0 to 10) {
    let fi = float_of_int(i);
    let scale = 3.14159 *. 2. /. 20.;
    let ti = fi *. 2. +. 1.5 +. 10.;
    let wall = Mazere.Border.Arc((400., 400., 300., ti *. scale, (ti +. 1.) *. scale));
    items := [wall, ...items^];
  };
  let walls = items^; */
  let tileCenter = (pos) => Board.tile_center(state.shape, state.scale, Board.from_point(state.shape, state.scale, pos));
  let player = switch curPos {
  | Some(pos) => tileCenter(Geom.tuple(pos))
  | None => Man.randomCoord(state);
  };
  let goal = Man.randomCoord(state);

  /* let maze = Maze({
    state,
    tileCenter: (state, pos) => Board.tile_center(state.shape, state.scale, Board.from_point(state.shape, state.scale, pos))
  }); */

  (walls, player, goal, tileCenter);
};

let newGame = state => {
  let (walls, (px, py), target, tileCenter) = makeMaze(Some(state.player.pos));
  {...state,
    walls,
    player: {pos: {Geom.x: px, y: py}, vel: Geom.v0, size: 10.}, target,
    path: Shared.LineSet.empty,
    currentPos: (px, py),
    tileCenter,
    /* maze, */
    throwTimer: Timer.fill(state.throwTimer)
  }
};

let speed = 0.5;
let keys = [
  (Events.Left, (-. speed, 0.)),
  (Events.Right, (speed, 0.)),
  (Events.Up, (0., -. speed)),
  (Events.Down, (0., speed)),
];

let maxSpeed = 7.;

/* let lineRect = ((a, b), (c, d), stroke) => {
  let hs = stroke /. 2.;
  if (a <= c && b <= d) {
    ((a -. hs, b -. hs), (max(c -. a, stroke)), max(d -. b, stroke))
  } else {
    ((c -. hs, d -. hs), (max(a -. c, stroke)), max(b -. d, stroke))
  }
}; */

let collide = (playerSize, pos, vel, walls) => {
  List.fold_left(
    (vel, wall) => switch wall {
    | Mazere.Border.Arc((cx, cy, r, t1, t2)) => {
      let c = Geom.Circle.{rad: playerSize, center: Geom.addVectorToPoint(vel, pos)};
      let arc = Geom.Arc.{cx, cy, r, t1, t2};
      if (Geom.Arc.testCircle(arc, c)) {
        let add = Geom.Arc.vectorToCircle(arc, c) |> Geom.pectorToVector |> Geom.invertVector;
        let add = Geom.{magnitude: add.magnitude -. playerSize, theta: add.theta};
        Geom.addVectors(add, vel);
      } else {
        vel
      }
    }
    | Line(((x1, y1), (x2, y2))) => {
      let c = Geom.Circle.{rad: playerSize, center: Geom.addVectorToPoint(vel, pos)};
      let p1 = {Geom.x: x1, y: y1};
      let p2 = {Geom.x: x2, y: y2};
      if (Geom.Circle.testLine(c, p1, p2)) {
        let add = Geom.Circle.vectorToLine(c, p1, p2) |> Geom.pectorToVector;
        let add = Geom.{magnitude: add.magnitude -. playerSize, theta: add.theta};
        Geom.addVectors(add, vel);
      } else {
        vel
      }
      /* let (pos, w, h) = lineRect(p1, p2, 6.);
      let intersect = Reprocessing.Utils.intersectRectCircle(~rectPos=pos, ~rectH=h, ~rectW=w, ~circleRad=Shared.playerSize);
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
      } */
    }
    },
    vel,
    walls
  )
};

let dist = ((x, y), (px, py)) => sqrt((px -. x) *. (px -. x) +. (py -. y) *. (py -. y));

let normalizePath = (p1, p2) => p1 > p2 ? (p1, p2) : (p2, p1);

let movePlayer = ({player: {pos, vel, size}, walls}, env) => {

  let (ax, ay, any) = List.fold_left(
    ((dx, dy, any), (k, (ax, ay))) => {
      if (Env.key(k, env)) {
        (dx +. ax, dy +. ay, true)
      } else {
        (dx, dy, any)
      }
    },
    (0., 0., false),
    keys
  );

  let slow = 0.8;
  let med = 0.9;
  let acc = Geom.pectorToVector({Geom.dx: ax, dy: ay});
  let vel = acc.Geom.magnitude < 0.001 ? vel : Geom.addVectors(vel, acc);
  let vel = Geom.scaleVector(vel, med);
  let vel = Geom.clampVector(vel, maxSpeed);
  /* let dx = ax == 0. ? player.dx *. slow : max(min(maxSpeed, player.dx +. ax), -. maxSpeed) *. med;
  let dy = ay == 0. ? player.dy *. slow : max(min(maxSpeed, player.dy +. ay), -. maxSpeed) *. med; */
  let vel = collide(size, pos, vel, walls);
  /* let x = player.x +. dx;
  let y = player.y +. dy; */
  {pos: Geom.addVectorToPoint(vel, pos), vel, size}
};

let step = ({player, walls, target} as state, env) => {

  let state = if (Env.keyPressed(Events.Space, env) && Timer.percent(state.throwTimer) > 0.1) {
    let height = Timer.percent(state.throwTimer);
    {
      ...state,
      throwTimer: Timer.restart(state.throwTimer),
      throwing: Some((Timer.createEmpty(height *. 2.), height)),
      player: {...player, vel: Geom.v0}
    }
  } else {
    state
  };

  let player = switch state.throwing {
  | None => movePlayer(state, env);
  | _ => state.player
  };

  let state = {
    ...state,
    throwTimer: Timer.inc(state.throwTimer, env),
    throwing: switch state.throwing {
    | None => None
    | Some((timer, height)) when Timer.isFull(timer) => None
    | Some((timer, height)) => Some((Timer.inc(timer, env), height))
    }
  };

  let pos = state.tileCenter((player.pos.Geom.x, player.pos.Geom.y));
  /* Printf.printf("%f, %f - %f %f\n", fst(pos), snd(pos), x, y); */
  /* Format.print_flush(); */
  let state = if (pos != state.currentPos) {
    {
      ...state,
      currentPos: pos,
      path: Shared.LineSet.add(normalizePath(state.currentPos, pos), state.path)
    }
  } else {
    state
  };

  if (dist(target, (player.pos.Geom.x, player.pos.Geom.y)) < player.size) {
    AnimateIn(Some(state), newGame(state), Timer.createEmpty(Shared.animateTime));
  } else {
    Playing({...state, player})
  }
};

let step = ({status} as context, env) => {
  {...context, status: switch status {
  | Playing(state) => if (Env.keyPressed(Events.Escape, env)) {
    AnimateIn(None, newGame(state), Timer.createEmpty(Shared.animateTime));
  } else {
    step(state, env)
  }
  | AnimateIn(prevState, state, timer) when Timer.isFull(timer) => Playing(state)
  | AnimateIn(prevState, state, timer) => AnimateIn(prevState, state, Timer.inc(timer, env))
  | _ => status
  }}
};