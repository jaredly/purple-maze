open Play_types;
open Reprocessing;

let purple = Reprocessing.Utils.color(~r=255, ~g=0, ~b=255, ~a=255);
let darker = Reprocessing.Utils.color(~r=200, ~g=0, ~b=255, ~a=255);
let shadow = Reprocessing.Utils.color(~r=150, ~g=0, ~b=200, ~a=100);
let background = Utils.color(~r=255, ~g=220, ~b=255, ~a=255);
let pathColor = Utils.color(~r=255, ~g=100, ~b=255, ~a=255);
/* let pathColor = Utils.color(~r=245, ~g=0, ~b=255, ~a=255); */

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {Reprocessing_Common.r, g, b, a: a *. alpha};



let draw_wall = (ctx, textFont, (xm, ym), wall) =>
  switch wall {
  | Mazere.Border.Line(((x, y), (a, b))) => Reprocessing.Draw.linef(~p1=(x +. xm, y +. ym), ~p2=(a +. xm, b +. ym), ctx)
  | Mazere.Border.Arc((x, y, r, t1, t2)) =>
    Reprocessing.Draw.noFill(ctx);
    Reprocessing.Draw.arcf(~center=(x +. xm, y +. ym), ~radx=r, ~rady=r, ~start=t1, ~stop=t2, ~isOpen=true, ~isPie=false, ctx);
  };

let drawPower = (jumpTimer, env) => {
  Draw.strokeWeight(1, env);
  Draw.noFill(env);
  Draw.stroke(withAlpha(Constants.black, 0.6), env);
  Draw.rectf(~pos=(10., 10.), ~width=20., ~height=100., env);

  Draw.noStroke(env);
  Draw.fill(withAlpha(Constants.black, 0.1), env);
  let height = 100. *. Timer.percent(jumpTimer);
  Draw.rectf(~pos=(10., 110. -. height), ~width=20., ~height=height, env);
};

let drawPlayerShadow = ({size, pos: {Geom.x, y}}, offset, env) => {
  Draw.noStroke(env);
  Draw.fill(shadow, env);
  let off = offset *. size *. 5.;
  let size = size;
  Draw.ellipsef(~center=(x +. off *. -0.3, y +. off), ~radx=size, ~rady=size, env);
};

let jumpPercent = ((timer, height)) => {
  let percent = Timer.percent(timer);
  sqrt(sin(percent *. 3.14159)) *. height;
};

let lightForMazeSize = size => {
  if (size <= 6) {
    6. /* 4 */
  } else if (size <= 8) {
    6. /* 5 */
  } else if (size <= 10) {
    6.
  } else if (size <= 13) {
    7.
  } else {
    8.
  }
};

let pi = 3.14159;
let tau = pi *. 2.;

let lightSize = (player, mazeSize) => player.size *. lightForMazeSize(mazeSize);

let drawJump = ({jumping, player} as state, env) => {
  /** Thrown light */
  switch jumping {
  | None => ()
  | Some((timer, height)) => {
    let p = jumpPercent((timer, height));
    let top = lightSize(player, state.mazeSize) +. player.size *. 10. *. p;
    let full = top;

    Draw.fill(background, env);
    let size = full;
    Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=size, ~rady=size, env);
  }
  };
};

let drawLights = ({player}, light, env) => {
  Draw.fill(background, env);
  /* let light = 800.; */
  Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=light, ~rady=light, env);
};

let drawFlashlight = ({player}, light, env) => {
  Draw.fill(background, env);
  /* let light = 800.; */
  /* Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=light, ~rady=light, env); */

  /* Mostly want a "mouse ever pressed" */
  let theta = Env.mousePressed(env)
    ? Geom.angleTo(player.pos, Geom.fromIntTuple(Env.mouse(env)))
    : player.vel.Geom.theta;
  Draw.arcf(
    ~center=Geom.tuple(player.pos),
    ~radx=light,
    ~rady=light,
    ~start=theta -. pi /. 6.,
    ~stop=theta +. pi /. 6.,
    ~isOpen=false,
    ~isPie=true,
    env
  )
};

let drawWalls = (walls, ~textFont=?, color, env) => {
  Draw.strokeWeight(3, env);
  Draw.stroke(color, env);
  List.iter(draw_wall(env, textFont, (0., 0.)), walls);
};

let drawPath = (state, env) => {
  Draw.strokeWeight(int_of_float(state.player.size /. 2.), env);
  Draw.stroke(pathColor, env);
  state.path |> LineSet.iter(((p1, p2)) => Draw.linef(~p1, ~p2, env));
  switch (MyQueue.peek(state.pendingPath)) {
  | None => ()
  | Some((p1, p2)) => {
    let pend = Geom.lerpTuples(p1, p2, Timer.percent(state.pathTimer));
    Draw.linef(~p1, ~p2=pend, env);
  }
  }
};

let pi = 3.14159;

let angleFrom = ((cx, cy), theta, magnitude) => {
  (
    cx +. cos(theta) *. magnitude,
    cy +. sin(theta) *. magnitude
  )
};

let outlineStar = (~pos, ~size, env) => {
  let angle = pi /. 5.;
  let offset = -. pi /. 2.;
  let insize = size *. 0.5;
  for (i in 0 to 4) {
    let fi = float_of_int(i);
    let theta = offset +. fi *. angle *. 2.;
    let p1 = angleFrom(pos, theta, size);
    let p2 = angleFrom(pos, theta -. angle, insize);
    let p3 = angleFrom(pos, theta +. angle, insize);
    Draw.linef(~p1, ~p2=p3, env);
    Draw.linef(~p1, ~p2, env);
  }
};

let fillStar = (~pos, ~size, ~angle, env) => {
  let offset = -. pi /. 2. +. angle;
  let angle = pi /. 5.;
  let insize = size *. 0.5;
  for (i in 0 to 4) {
    let fi = float_of_int(i);
    let theta = offset +. fi *. angle *. 2.;
    let p1 = angleFrom(pos, theta, size);
    let p2 = angleFrom(pos, theta -. angle, insize);
    let p3 = angleFrom(pos, theta +. angle, insize);
    Draw.trianglef(~p1, ~p2, ~p3, env);
    Draw.trianglef(~p1=pos, ~p2, ~p3, env);
  }
};

let innerStar = (~pos, ~size, ~alpha, env) => {
  Draw.fill(withAlpha(Reprocessing.Utils.color(~r=255, ~g=200, ~b=100, ~a=255), alpha), env);
  fillStar(~pos, ~size, env);
};

let goldStar = (~pos, ~size, ~alpha, ~angle, env) => {
  Draw.fill(withAlpha(Reprocessing.Utils.color(~r=255, ~g=180, ~b=80, ~a=255), alpha), env);
  Draw.fill(withAlpha(Reprocessing.Utils.color(~r=255, ~g=154, ~b=0, ~a=255), alpha), env);
  fillStar(~pos, ~size=size, ~angle, env);

  innerStar(~pos, ~size=size /. 1.5, ~alpha, ~angle, env);
};

let perfectStar = (~pos, ~size, ~alpha, ~angle, env) => {
  /* Draw.fill(Reprocessing.Utils.color(~r=255, ~g=180, ~b=80, ~a=255), env);
  Draw.fill(Constants.white, env); */
  Draw.fill(withAlpha(purple, alpha), env);
  fillStar(~pos, ~size=size, ~angle, env);

  innerStar(~pos, ~size=size /. 1.5, ~alpha, ~angle, env);
};

let starPositions = (stars, center, playerSize) => {
  let dist = playerSize *. 2.;
  let centerAngle = -. pi /. 2.;
  let at = (offset) => (offset, angleFrom(center, centerAngle +. offset, dist));
  switch stars {
  | One => [at(0.)];
  | Two | TwoSlow => [
    at(pi /. 7.),
    at(-. pi /. 7.),
  ]
  | Three => [
    at(0.),
    at(pi /. 3.),
    at(-. pi /. 3.),
  ]
  }
};

let drawScore = (player, score, scale, alpha, env) => {
  let positions = starPositions(score.stars, Geom.tuple(player.pos), player.size /. 2. *. scale);
  let size = player.size *. 0.5 *. scale;
  positions |> List.iter(((angle, pos)) => {
    (score.pathLength <= 1.  && score.stars == Three ? perfectStar : goldStar)(
      ~pos,
      ~size,
      ~alpha,
      ~angle,
      env
    )
  })
};

let drawPlayer = (player, scale, jumpTimer, env) => {
  Draw.noStroke(env);
  Draw.fill(darker, env);
  let size = player.size *. scale;
  Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=size, ~rady=size, env);

  Draw.stroke(Reprocessing.Utils.color(~r=0, ~g=0, ~b=0, ~a=50), env);
  Draw.strokeWeight(int_of_float(size /. 4.), env);
  Draw.noFill(env);
  Draw.arcf(
    ~center=Geom.tuple(player.pos),
    ~radx=size *. 0.8,
    ~rady=size *. 0.8,
    ~start=0.,
    ~stop=Timer.percent(jumpTimer) *. tau,
    ~isPie=false,
    ~isOpen=true,
    env
  );
  Draw.noStroke(env);
};

let drawGoal = (size, target, alpha, env) => {
  Draw.stroke(withAlpha(background, alpha), env);
  Draw.strokeWeight(5, env);
  Draw.fill(withAlpha(purple, alpha), env);
  Draw.ellipsef(~center=target, ~radx=size, ~rady=size, env);
};

let drawStatus = (textFont, state, env) => {
  let count = LineSet.cardinal(state.path);
  let ratio = float_of_int(count) /. float_of_int(state.goalDistance);
  /* let rs = string_of_float(ratio); */
  Draw.text(~font=textFont, ~body=
  Printf.sprintf(
    "%d / %d : %0.3f",
    count, state.goalDistance, ratio
  )
  , ~pos=(10, 10), env);

  let time = (Env.getTimeMs(env) -. state.startTime) /. 1000.;
  Draw.text(~font=textFont, ~body=Printf.sprintf("%0.3f",  float_of_int(count) /. time), ~pos=(10, 30), env);
};

let draw = ({player, walls, target, jumpTimer, jumping, path} as state, {Shared.textFont, width, height}, env) => {
  Draw.background(purple, env);

  /* drawLights(state, lightSize(player, state.mazeSize), env); */
  drawFlashlight(state, lightSize(player, state.mazeSize), env);
  drawJump(state, env);
  drawWalls(state.walls, ~textFont, purple, env);
  drawPath(state, env);
  drawGoal(player.size, state.target, 1., env);
  drawPlayer(state.player, switch jumping {
  | None => 1.
  | Some((timer, height)) => {
    let p =jumpPercent((timer, height));

    drawPlayerShadow(state.player, p, env);
    1. +. p
  }
  }, state.jumpTimer, env);

  /* drawPower(jumpTimer, env); */
  /* This was just for debugging I think */
  drawStatus(textFont, state, env);

  /* For debugging the maze algorithms */

  /* Draw.tint(Constants.black, env);
  state.coords |> Array.iter((((cx, cy), (x, y))) => {
    Draw.text(~font=textFont, ~body=Printf.sprintf("%d,%d", cx, cy), ~pos=(int_of_float(x), int_of_float(y)), env)
  }); */

  /* For debugging the from_point function */
  /* let trace = state.tileCenter(player.pos |> Geom.tuple);
  Draw.ellipsef(~center=trace, ~radx=5., ~rady=5., env); */

  /* state.distances |> Array.iter((((x, y), i)) => {
    Draw.text(~font=textFont, ~body=Printf.sprintf("%d", i), ~pos=(int_of_float(x), int_of_float(y)), env)
  }); */

  ()
};

let flyIn = (state, percent, env) => {
  let an = cos(percent *. 3.14159 /. 2.);
  let size = (1000. -. lightSize(state.player, state.mazeSize)) *. an +. lightSize(state.player, state.mazeSize);

  drawLights(state, size, env);
  drawWalls(state.walls, purple, env);
  drawGoal(state.player.size, state.target, 1., env);
  drawPlayerShadow(state.player, an, env);
  drawPlayer(state.player, an +. 1., state.jumpTimer, env);

  /* drawPower(state.jumpTimer, env); */
};

let flyOut = (state, nextState, percent, score, context, env) => {
  let an = 1. -. percent;
  let an = cos(an *. 3.14159 /. 2.);
  let size = (1000. -. lightSize(state.player, state.mazeSize)) *. an +. lightSize(state.player, state.mazeSize);

  drawLights(state, size, env);
  let over = Geom.Ease.easeOutQuad(min(1., percent *. 2.));
  drawWalls(state.walls, withAlpha(purple, 1. -. over), env);
  drawWalls(nextState.walls, withAlpha(purple, over), env);
  drawGoal(state.player.size, state.target, 1. -. over, env);
  drawGoal(nextState.player.size, nextState.target, over, env);

  let player = {
    ...state.player,
    size: Geom.lerp(state.player.size, nextState.player.size, percent),
    pos: Geom.lerpPos(state.player.pos, nextState.player.pos, percent)
  };

  drawPlayerShadow(player, an, env);
  drawPlayer(player, an +. 1., state.jumpTimer, env);

  let fade = percent > 0.7 ? Geom.Ease.easeInQuad((1.0 -. percent) /. 0.3) : 1.;
  drawScore(player, score, an +. 1., fade, env);


  /* drawPower(state.jumpTimer, env); */

  /* let (x, y) = Geom.intTuple(state.player.pos);
  Draw.text(~font=context.Shared.titleFont, ~body=switch score.stars {
  | One => "1 - Done!"
  | Two => "2 - Good!"
  | TwoSlow => "2 - try to go faster"
  | Three => score.pathLength == 1. ? "3 - Perfect!!" : "3 - Awesome"
  }, ~pos=(x, y < 50 ? y + 20 : y - 20), env); */
};

/** TODO have a state vbl "using keyboard" and if you do, the joystick disappears */
let drawJoystick = (env) => {
  let width = Reprocessing.Env.width(env) |> float_of_int;
  let height = Reprocessing.Env.height(env) |> float_of_int;
  let size = Play_step.joystickSize;
  let margin = Play_step.joystickMargin;
  Draw.fill(Reprocessing.Utils.color(~r=100, ~g=0, ~b=100, ~a=50), env);
  Draw.ellipsef(~center=(size +. margin, height -. size -. margin), ~radx=size, ~rady=size, env);
  Draw.stroke(Reprocessing.Utils.color(~r=100, ~g=0, ~b=100, ~a=100), env);
  Draw.noFill(env);
  Draw.strokeWeight(3, env);
  /** TODO maybe a hexagon instead */
  Draw.ellipsef(~center=(size +. margin, height -. size -. margin), ~radx=size /. 3., ~rady=size /. 3., env);
  Draw.noStroke(env);
};

let draw = (status, context, env) => {
  switch status {
  | Playing(state) => draw(state, context, env)
  | AnimateIn(prevState, state, timer) => {
      Draw.background(purple, env);
      switch prevState {
      | None => flyIn(state, Timer.percent(timer), env)
      | Some((prevState, outTimer, score)) => {
        if (!Timer.isFull(outTimer)) {
          flyOut(prevState, state, Timer.percent(outTimer), score, context, env)
        } else {
          flyIn(state, Timer.percent(timer), env)
        }
      }
      }
  }
  };
  /* drawJoystick(env); */
};