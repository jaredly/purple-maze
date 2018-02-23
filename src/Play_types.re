
type line = ((float, float), (float, float));

let module LineSet = Set.Make({
  type t = line;
  let compare = compare;
});

type player = {pos: Geom.point, vel: Geom.vector, size: float};

type state = {
  walls: list(Mazere.Border.t),
  coords: array((string, (float, float))),
  distances: array(((float, float), int)),
  goalDistance: int,
  path: LineSet.t,
  pathTimer: Timer.t,
  startTime: float,
  pendingPath: MyQueue.t(line),
  mazeSize: int,
  currentPos: (float, float),
  tileCenter: ((float, float)) => (float, float),
  /* maze: mazeG, */
  player,
  target: (float, float),
  jumpTimer: Timer.t,
  jumping: option((Timer.t, float)),
  time: float,
};

type stars = One | Two | TwoSlow | Three;

type score = {
  avgSpeed: float,
  pathLength: float,
  stars,
};

type status =
  | AnimateIn(option((state, Timer.t, score)), state, Timer.t)
  | Playing(state)
  ;