open Play_types;
open Reprocessing;


let pathTime = 0.1;

let animateOutTime = 1.;

let animateInForSize = size => {
  if (size <= 6) {
    1.
  } else if (size <= 8) {
    1.3
  } else if (size <= 10) {
    2.
  } else if (size <= 13) {
    3.
  } else {
    4.
  }
};

let makeMaze = (~size=10, curPos, env) => {
  /* Not so much */
  /* let module Board = Mazere.HexBox; */
  /* let module Board = Mazere.NewHexTriangle; */
  /* Good */
  /* let module Board = Mazere.NewRect; */
  /* let module Board = Mazere.TriangleBoard; */
  /* let module Board = Mazere.SquareTriangle; */
  /* let module Board = Mazere.Circle; */
  /* let module Board = Mazere.FourSquare; */
  let modules: array((module Mazere.SimpleBoard.T)) = [|
    (module Mazere.NewRect),
    (module Mazere.SquareTriangle),
    /* (module Mazere.TriangleBoard), */
    /* (module Mazere.Circle), */
    (module Mazere.FourSquare),
    (module Mazere.TriangleBox),
  |];
  let module Board = (val (modules[Random.int(Array.length(modules))]));
  /* let module Board = Mazere.Circle; */
  let module Alg = Mazere.NewDepth.F(Mazere.NewDepth.RandomConfig({}));

  let module Man = Mazere.Manager.F(Board, Alg);

  Man.randInit();

  let height = Reprocessing.Env.height(env) |> float_of_int;
  let width = Reprocessing.Env.width(env) |> float_of_int;

  /* let (width, height) = (800., 800.); */
  let min_margin = 10.;
  /* let size_hint = 6;
  let size_hint = 8; */
  let size_hint = size;

  let with_margins = (width -. min_margin *. 2.0, height -. min_margin *. 2.0);
  let state = Man.init(with_margins, size_hint);
  let state = Man.loop_to_end(state);

  let (boardWidth, boardHeight) = state.outsize;
  let offX = (width -. boardWidth) /. 2.;
  let offY = (height -. boardHeight) /. 2.;
  let offset = ((x, y)) => (x +. offX, y +. offY);
  let backset = ((x, y)) => (x -. offX, y -. offY);

  let offsetWall = wall => switch wall {
  | Mazere.Border.Arc((cx, cy, a, b, c)) => Mazere.Border.Arc((cx +. offX, cy +. offY, a, b, c))
  | Line(((a, b), (c, d))) => Mazere.Border.Line(((a +. offX, b +. offY), (c +. offX, d +. offY)))
  };

  let walls = Man.all_walls(state) |> List.map(offsetWall);
  /* let walls = []; */
  let tileCenter = (pos) => Board.tile_center(state.shape, state.scale, Board.from_point(state.shape, state.scale, backset(pos))) |> offset;
  let playerCoord = switch curPos {
  | Some(pos) => {
    let coord = Board.from_point(state.shape, state.scale, Geom.tuple(pos));
    if (Man.isCoordInBoard(state, coord)) {
      coord
      /* Board.tile_center(state.shape, state.scale, coord) */
    } else {
      Man.randomCoord(state)
    }
  }
  | None => Man.randomCoord(state);
  };

  let distances = Man.distanceFromCoord(state, playerCoord)
  |> Array.map(((coord, i)) => (Board.tile_center(state.shape, state.scale, coord) |> offset, i));

  let player = Man.tileCenter(state, playerCoord) |> offset;
  let max = Array.fold_left((m, (_, dist)) => max(m, dist), 0, distances);
  let targetDist = max * 3 / 4;
  let potentialGoals = Array.fold_left((matching, (pos, dist)) => dist == targetDist ? [pos, ...matching] : matching, [], distances);
  let count = List.length(potentialGoals);
  let goal = List.nth(potentialGoals, Random.int(count));

  let coords = Man.allCoords(state);

  let playerSize = state.scale /. 4.;
  /*
    To veirfy that the from_point calculation is correct.
    coords |> Array.iter(((coord, pos)) => {
    let c2 = Board.from_point(state.shape, state.scale, pos);
    if (!Man.isCoordInBoard(state, coord)) {
      failwith("Coord I got from allCorrds not in board");
    } else if (coord != c2) {
      print_endline("Roundtrip to from_point gives a different coord: " ++ Board.Coord.show(coord) ++ " became " ++ Board.Coord.show(c2));
    } else {
      ()
    }
  }); */

  let coords = coords |> Array.map(((coord, pos)) => (Board.Coord.show(coord), pos));
  /* (walls, player, goal, tileCenter, coords, distances); */
  {
      tileCenter,
      coords,
      distances,
      goalDistance: targetDist,
      pathTimer: Timer.createEmpty(pathTime),
      pendingPath: MyQueue.empty,
      player: {
        pos: Geom.fromTuple(player),
        vel: Geom.v0,
        size: playerSize,
      },
      mazeSize: size,
      startTime: Reprocessing.Env.getTimeMs(env),
      target: goal,
      walls,
      path: LineSet.empty,
      currentPos: player,
      jumpTimer: Timer.createFull(10.),
      jumping: None,
      time: 0.,
    }
};

let initialState = (size, env) => {
  Random.self_init();
  makeMaze(~size, None, env);
};

let newGame = (~size, state, env) => {
  makeMaze(~size, Some(state.player.pos), env);
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

let normalizePath = ((p1, p2)) => compare(p1, p2) > 0 ? (p1, p2) : (p2, p1);

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

  let multiplier = size /. 15.;

  let oneFrame = 1.0 /. 60.;
  let speed = Reprocessing.Env.deltaTime(env) /. oneFrame;

  let slow = 0.8;
  let med = 0.9;
  let acc = Geom.scaleVector(Geom.pectorToVector({Geom.dx: ax, dy: ay}), multiplier);
  let vel = acc.Geom.magnitude < 0.001 ? vel : Geom.addVectors(vel, acc);
  let vel = Geom.scaleVector(vel, med);
  let vel = Geom.clampVector(vel, maxSpeed *. multiplier);

  /* let dx = ax == 0. ? player.dx *. slow : max(min(maxSpeed, player.dx +. ax), -. maxSpeed) *. med;
  let dy = ay == 0. ? player.dy *. slow : max(min(maxSpeed, player.dy +. ay), -. maxSpeed) *. med; */
  let vel = collide(size, pos, vel, walls);
  /* let x = player.x +. dx;
  let y = player.y +. dy; */
  /* Geom.scaleVector(vel, speed) */
  {pos: Geom.addVectorToPoint(vel, pos), vel, size}
};

let step = ({player, walls, target} as state, env) => {

  let state = if (Env.keyPressed(Events.Space, env) && Timer.percent(state.jumpTimer) > 0.1) {
    let height = Timer.percent(state.jumpTimer);
    {
      ...state,
      jumpTimer: Timer.restart(state.jumpTimer),
      jumping: Some((Timer.createEmpty(height *. 2.), height)),
      player: {...player, vel: Geom.v0}
    }
  } else {
    state
  };

  let player = switch state.jumping {
  | None => movePlayer(state, env);
  | _ => state.player
  };

  let state = {
    ...state,
    jumpTimer: Timer.inc(state.jumpTimer, env),
    jumping: switch state.jumping {
    | None => None
    | Some((timer, height)) when Timer.isFull(timer) => None
    | Some((timer, height)) => Some((Timer.inc(timer, env), height))
    }
  };

  let pos = state.tileCenter((player.pos.Geom.x, player.pos.Geom.y));
  /* Printf.printf("%f, %f - %f %f\n", fst(pos), snd(pos), x, y); */
  /* Format.print_flush(); */
  let state = if (pos != state.currentPos && !LineSet.mem(normalizePath((state.currentPos, pos)), state.path)) {
    {
      ...state,
      currentPos: pos,
      pathTimer: if (MyQueue.isEmpty(state.pendingPath)) {
        Timer.createEmpty(pathTime)
      } else {
        state.pathTimer
      },
      pendingPath: MyQueue.add((state.currentPos, pos), state.pendingPath)
    }
  } else {
    {...state, currentPos: pos}
  };

  let (pathTimer, isFull) = Timer.incLoop(state.pathTimer, env);
  let (path, pendingPath) = isFull
    ? {
      switch (MyQueue.take(state.pendingPath)) {
      | None => (state.path, state.pendingPath)
      | Some((el, pendingPath)) => (LineSet.add(normalizePath(el), state.path), pendingPath)
      }
    }
    : (state.path, state.pendingPath);
  let pendingPath = switch (MyQueue.take(pendingPath)) {
  | Some((item, rest)) when LineSet.mem(normalizePath(item), state.path) => rest
  | _ => pendingPath
  };
  let state = { ...state, pathTimer, path, pendingPath };

  if (dist(target, (player.pos.Geom.x, player.pos.Geom.y)) < player.size *. 1.5) {
    /** TODO add in last piece of path */
    let isAtTarget = pos == state.tileCenter(target);
    let left = MyQueue.dump(state.pendingPath);
    let path = List.fold_left(
      ((path, item) => LineSet.add(normalizePath(item), path)),
      state.path,
      isAtTarget ? left : [(pos, state.tileCenter(target)), ...left]
    );
    `Won({...state, path})
  } else {
    `Continue({...state, player})
  }
};

let step = (status, context, env) => {
  switch status {
  | Playing(state) =>
    switch (step(state, env)) {
    | `Continue(state) => `Continue(Playing(state))
    | `Won(state) => {
      `Won(state)
    }
    }
  | AnimateIn(Some((prevState, outTimer, score)), state, inTimer) when !Timer.isFull(outTimer) => `Continue(AnimateIn(Some((prevState, Timer.inc(outTimer, env), score)), state, inTimer))
  | AnimateIn(prevState, state, timer) when Timer.isFull(timer) => `Continue(Playing(state))
  | AnimateIn(prevState, state, timer) => `Continue(AnimateIn(prevState, state, Timer.inc(timer, env)))
  }
};

let start = (boardSize, env) => {
  AnimateIn(None, initialState(boardSize, env), Timer.createEmpty(animateInForSize(boardSize)));
};

let stars = (pathLength, avgSpeed) => {
  if (pathLength < 1.15) {
    avgSpeed < 1.5 ? TwoSlow : Three
  } else if (pathLength < 1.5) {
    Two
  } else {
    One
  }
};

let continue = (prevState, boardSize, env) => {
  let count = LineSet.cardinal(prevState.path);
  let ratio = float_of_int(count) /. float_of_int(prevState.goalDistance);

  /* Printf.printf("Finished %d %d %0.2f\n", count, prevState.goalDistance, ratio);
  LineSet.iter((((x1, y1), (x2, y2))) => {
    Printf.printf("%0.2f, %0.2f - %0.2f, %0.2f\n", x1, y1, x2, y2);
  }, prevState.path);
  let (x, y) = prevState.target;
  Printf.printf("Finished %0.2f %0.2f\n", x, y);
  print_newline(); */

  let time = (Env.getTimeMs(env) -. prevState.startTime) /. 1000.;
  let avgSpeed = float_of_int(count) /. time;
  let score = {
    pathLength: ratio,
    avgSpeed,
    stars: stars(ratio, avgSpeed)
  };
  AnimateIn(Some((prevState, Timer.createEmpty(animateOutTime), score)), newGame(~size=boardSize, prevState, env), Timer.createEmpty(animateInForSize(boardSize)));
};
