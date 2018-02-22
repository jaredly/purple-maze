
let fakePhone = try {Sys.getenv("PHONE") |> ignore; true} { | Not_found => false };
let isPhone = Reprocessing.target == "native-ios" || Reprocessing.target == "native-android" || fakePhone;

type platform = {x: float, y: float, w: float};

type input = Left | Right | NoInput;

type player = {pos: Geom.point, vel: Geom.vector, size: float};

/* let playerSize = 20.; */

/* type maze('state, 'coord) = {
  state: 'state,
  /* toPoint: ('state, 'coord) => (float, float),
  fromPoint: ('state, (float, float)) => 'coord, */
  tileCenter: ('state, (float, float)) => (float, float),
};

type mazeG = | Maze(maze('state, 'coord)): mazeG; */

let module LineSet = Set.Make({
  type t = ((float, float), (float, float));
  let compare = compare;
});

type state = {
  walls: list(Mazere.Border.t),
  path: LineSet.t,
  currentPos: (float, float),
  tileCenter: ((float, float)) => (float, float),
  /* maze: mazeG, */
  player,
  target: (float, float),
  throwTimer: Timer.t,
  throwing: option((Timer.t, float)),
  time: float,
};

let animateTime = 2.;

type status =
  | Start
  | AnimateIn(option(state), state, Timer.t)
  | Playing(state)
  | Done(float);

type context = {
  status,
  height: float,
  width: float,
  smallFont: Reprocessing.fontT,
  textFont: Reprocessing.fontT,
  titleFont: Reprocessing.fontT,
  boldTextFont: Reprocessing.fontT,
  smallTitleFont: Reprocessing.fontT,
};
