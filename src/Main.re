
let getEnv = name => try { Some(Sys.getenv(name)) } { | Not_found => None };

let (/+) = Filename.concat;
let setup = (assetDir, env) => {
  if (!Shared.isPhone) {
    Reprocessing.Env.resizeable(false, env);
  };

  Reprocessing.Env.size(~width=500, ~height=500, env);
  /* if (!Shared.isPhone) {
    let size = min(Reprocessing.Env.maxHeight(env), 800);
    Reprocessing.Env.size(~width=size / 2, ~height=size, env);
  } else if (Shared.fakePhone) {
    switch (getEnv("TABLET")) {
    | Some("7") => Reprocessing.Env.size(~width=600, ~height=1024, env)
    | Some("10") => Reprocessing.Env.size(~width=800, ~height=1280, env)
    | Some("ios") => Reprocessing.Env.size(~width=1242/4 + 1, ~height=2208/4, env)
    | Some("iostab") => Reprocessing.Env.size(~width=2048/4 + 1, ~height=2732/4 + 1, env)
    | Some(_)
    | None => Reprocessing.Env.size(~width=340, ~height=640, env)
    }
  }; */

  let height = Reprocessing.Env.height(env) |> float_of_int;
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let platforms = [];
  /* Step.randomPlatforms(width, height); */

  /* let start = List.nth(platforms, 10); */
  {
    status: Start,
    user: (0., 0., 0., 0.),
    /* (start.x, start.y -. 10., 0., 0.), */
    prevInput: NoInput,
    walls: Step.makeMaze(),
    height,
    width,
    Shared.titleFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Black-48.fnt", ~isPixel=false, env),
    smallTitleFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-24.fnt", ~isPixel=false, env),
    boldTextFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Black-24.fnt", ~isPixel=false, env),
    textFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-24.fnt", ~isPixel=false, env),
    smallFont: Reprocessing.Draw.loadFont( ~filename=assetDir /+ "Orbitron-Regular-16.fnt", ~isPixel=false, env),
  }
};

let draw = (state, env) => {
  let state = Step.step(state, env);
  IcyDraw.draw(state, env);
  state
};

let run = (assetDir, _) => Reprocessing.run(
  ~setup=setup(assetDir),
  ~mouseDown=((state, env) => {
    switch (state.status) {
    | Start => {...state, status: Playing(0.)}
    | Done(_) => Step.newGame(state)
    | _ => state
    }
  }),
  ~draw,
  ()
);