
let fakePhone = try {Sys.getenv("PHONE") |> ignore; true} { | Not_found => false };
let isPhone = Reprocessing.target == "native-ios" || Reprocessing.target == "native-android" || fakePhone;

type platform = {x: float, y: float, w: float};

type input = Left | Right | NoInput;

type status =
  | Start
  | Playing(float)
  | Done(float);

type context = {
  platforms: list(platform),
  user: (float, float, float, float),
  status,
  height: float,
  width: float,
  prevInput: input,
  smallFont: Reprocessing.fontT,
  textFont: Reprocessing.fontT,
  titleFont: Reprocessing.fontT,
  boldTextFont: Reprocessing.fontT,
  smallTitleFont: Reprocessing.fontT,
};

let getDistance = status => switch status {
| Playing(d) => d
| Done(d) => d
| Start => 0.
};