
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


module Queue = {
  type t('a) = ((list('a), list('a)));
  let empty = ([], []);
  let isEmpty = x => x == empty;
  let add = (x, (front, back)) => {
    ([x, ...front], back);
  };
  let create = x => add(x, empty);
  let rec take = (queue) =>
    switch queue {
    | (front, [x]) => Some((x, ([], List.rev(front))))
    | (front, [x, ...back]) => Some((x, (front, back)))
    | ([], []) => None
    | (front, []) => take(([], List.rev(front)))
    };
  let peek = ((front, back)) => {
    switch back {
    | [x, ...rest] => Some(x)
    | [] => {
      let l = List.length(front);
      if (l > 0) {
        Some(List.nth(front, l - 1))
      } else {
        None
      }
    }
    }
  };
};

type line = ((float, float), (float, float));

let module LineSet = Set.Make({
  type t = line;
  let compare = compare;
});

let pathTime = 0.1;

type state = {
  walls: list(Mazere.Border.t),
  coords: array((string, (float, float))),
  distances: array(((float, float), int)),
  goalDistance: int,
  path: LineSet.t,
  pathTimer: Timer.t,
  startTime: float,
  pendingPath: Queue.t(line),
  currentPos: (float, float),
  tileCenter: ((float, float)) => (float, float),
  /* maze: mazeG, */
  player,
  target: (float, float),
  throwTimer: Timer.t,
  throwing: option((Timer.t, float)),
  time: float,
};

let animateOutTime = 1.;

let animateInForSize = size => {
  if (size <= 6) {
    1.
  } else if (size <= 8) {
    1.3
  } else if (size <= 10) {
    1.5
  } else if (size <= 13) {
    2.
  } else {
    2.5
  }
};

type status =
  | Start
  | AnimateIn(option((state, Timer.t)), state, Timer.t)
  | Playing(state)
  | Done(float);

type context = {
  status,
  height: float,
  width: float,
  mazeSize: int,
  smallFont: Reprocessing.fontT,
  textFont: Reprocessing.fontT,
  titleFont: Reprocessing.fontT,
  boldTextFont: Reprocessing.fontT,
  smallTitleFont: Reprocessing.fontT,
};
