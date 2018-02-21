
let fakePhone = try {Sys.getenv("PHONE") |> ignore; true} { | Not_found => false };
let isPhone = Reprocessing.target == "native-ios" || Reprocessing.target == "native-android" || fakePhone;

type platform = {x: float, y: float, w: float};

type input = Left | Right | NoInput;

type player = {x: float, y: float, dx: float, dy: float};

type state = {
  walls: list(Mazere.Border.t),
  player,
  target: (float, float),
  throwTimer: Timer.t,
  throwing: option((Timer.t, float)),
  time: float,
};

type status =
  | Start
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
