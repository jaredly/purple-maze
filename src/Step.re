open Shared;
open Reprocessing;

let rec findStandingBlock = (platforms, px, py, dy) => switch platforms {
| [] => None
| [{x, y, w}, ...rest] when x <= px +. 10. && x +. w >= px && py +. 10. >= y && py +. 10. <= y +. 2. +. dy => Some(y)
| [_, ...rest] => findStandingBlock(rest, px, py, dy)
};

let maxdx = 35.;

let randomPlatforms = (width, height) => List.map(
    i => {
      let w = Random.float(100.) +. 30.;
      {
        Shared.y: height -. float_of_int(i) *. 40.,
        w,
        x: Random.float(width -. w)
      }
    },
    MyUtils.range(1000)
  );

let newGame = state => {
  let platforms = randomPlatforms(state.width, state.height);
  let start = List.nth(platforms, 10);
  {
    ...state,
    status: Start,
    user: (start.x, start.y -. 10., 0., 0.),
    prevInput: NoInput,
    platforms
  }
};

let getInput = env => {
  if (Env.key(Events.Left, env)) {
    Left
  } else if (Env.key(Events.Right, env)) {
    Right
  } else if (Env.mousePressed(env)) {
    let (x, _) = Env.mouse(env);
    x > Env.width(env) / 2 ? Right : Left
  } else {
    NoInput
  }
};

/** Not using this right now */
let wallJump = (input, width, x, dx, dy) => {
  if (x < 0.) {
    if (input == Right) {
      (0., -. dx, dy -. abs_float(dx))
    } else {
      (0., -. dx *. 0.5, dy)
    }
  } else if (x +. 10. > width) {
    if (input == Left) {
      (width -. 10., -. dx, dy -. abs_float(dx))
    } else {
      (width -. 10., -. dx *. 0.5, dy)
    }
  } else {
    (x, dx, dy);
  };
};

let wallBounce = (width, x, dx, dy) => {
  if (x < 0.) {
    (0., -. dx, dy)
  } else if (x +. 10. > width) {
    (width -. 10., -. dx, dy)
  } else {
    (x, dx, dy);
  };
};

let gameStep = (distance, state, env) => {
  let d = Env.deltaTime(env) *. 10.;
  let factor = int_of_float(distance) / 1000;
  let distance = distance +. d +. (0.5 *. float_of_int(factor));
  let platforms = List.filter(platform => platform.y +. distance < state.height, state.platforms);

  let (x, y, dx, dy) = state.user;
  let y = y +. dy *. d;
  let x = x +. dx *. d;
  let onBlock = dy < 0. ? None : findStandingBlock(platforms, x, y, dy *. d);
  let hspeed = 3.5 *. d;
  /* (onBlock == None ? 1.5 : 2.5) *. d; */
  let friction = 0.98;
  /* 0.95; */
  let input = getInput(env);
  let (x, y, dx, dy) = switch (input) {
    | Left => (x, y, (onBlock == None && dx > 0. ? dx *. friction : dx) -. hspeed, dy)
    | Right => (x, y, (onBlock == None && dx < 0. ? dx *. friction : dx) +. hspeed, dy)
    | NoInput => (x, y, (onBlock == None ? dx : dx *. friction), dy)
  };

  let dx = max(-. maxdx, min(dx, maxdx));

  let (x, dx, dy) = wallBounce(state.width, x, dx, dy);

  let ax = abs_float(dx);
  let jumpSpeed = -30. -. if (ax > 10.) {
    ax *. 0.75
  } else {
    0.
  };

  /* maybe jump */
  let (y, dy) = switch onBlock {
  /* gravity */
  | None => (y, dy +. 5. *. d)
  | Some(by) => (
      by -. 10.,
      /* Are we about to fall off? */
      switch (findStandingBlock(platforms, x +. dx *. d, y, dy *. d)) {
      | None => jumpSpeed
      | Some(_) => input == NoInput && state.prevInput != NoInput ? jumpSpeed : 0.
      }
    )
  };

  let delta = (state.height /. 2. -. y) -. distance;
  let distance = delta > 0. ? distance +. delta /. 20. *. d : distance;
  let distance = max(distance, -. y);

  if (y +. distance > state.height -. 10.) {
    {...state, status: Done(distance)}
  } else {
    {
      ...state,
      prevInput: input,
      user: (x, y, dx, dy),
      platforms,
      status: Playing(distance)
    }
  }

};

let step = (state, env) => {
  let distance = getDistance(state.status);
  switch (state.status) {
  | Playing(distance) => {
    gameStep(distance, state, env)
  }
  | _ => state
  }
};
