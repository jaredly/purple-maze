open Shared;
open Reprocessing;

let purple = Reprocessing.Utils.color(~r=255, ~g=0, ~b=255, ~a=255);
let darker = Reprocessing.Utils.color(~r=200, ~g=0, ~b=255, ~a=255);
let shadow = Reprocessing.Utils.color(~r=150, ~g=0, ~b=200, ~a=100);
let background = Utils.color(~r=255, ~g=220, ~b=255, ~a=255);
let pathColor = Utils.color(~r=255, ~g=100, ~b=255, ~a=255);
/* let pathColor = Utils.color(~r=245, ~g=0, ~b=255, ~a=255); */

let withAlpha = ({Reprocessing_Common.r, g, b, a}, alpha) => {Reprocessing_Common.r, g, b, a: a *. alpha};

let drawPower = (throwTimer, env) => {
  Draw.strokeWeight(3, env);
  Draw.noFill(env);
  Draw.stroke(Constants.black, env);
  Draw.rectf(~pos=(10., 10.), ~width=20., ~height=100., env);

  Draw.noStroke(env);
  Draw.fill(Constants.black, env);
  let height = 100. *. Timer.percent(throwTimer);
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

let drawJump = ({throwing, player} as state, env) => {
  /** Thrown light */
  switch throwing {
  | None => ()
  | Some((timer, height)) => {
    let p = jumpPercent((timer, height));
    let top = 80. +. 200. *. p;
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

let drawWalls = (walls, ~textFont=?, color, env) => {
  Draw.strokeWeight(3, env);
  Draw.stroke(color, env);
  List.iter(DrawMaze.draw_wall(env, textFont, (0., 0.)), walls);
};

let drawPath = (state, env) => {
  Draw.strokeWeight(int_of_float(state.player.size /. 2.), env);
  Draw.stroke(pathColor, env);
  state.path |> Shared.LineSet.iter(((p1, p2)) => Draw.linef(~p1, ~p2, env));
  switch (Shared.Queue.peek(state.pendingPath)) {
  | None => ()
  | Some((p1, p2)) => {
    let pend = Geom.lerpTuples(p1, p2, Timer.percent(state.pathTimer));
    Draw.linef(~p1, ~p2=pend, env);
  }
  }
};

let drawPlayer = (player, scale, env) => {
  Draw.noStroke(env);
  Draw.fill(darker, env);
  Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=player.size *. scale, ~rady=player.size *. scale, env);
};

let drawGoal = (size, target, color, env) => {
  Draw.stroke(background, env);
  Draw.strokeWeight(5, env);
  Draw.fill(color, env);
  Draw.ellipsef(~center=target, ~radx=size, ~rady=size, env);
};

let draw = ({player, walls, target, throwTimer, throwing, path} as state, {textFont, width, height}, env) => {
  Draw.background(purple, env);
  drawLights(state, 80., env);
  drawJump(state, env);
  drawWalls(state.walls, ~textFont, purple, env);
  drawPath(state, env);
  drawGoal(player.size, state.target, purple, env);
  drawPlayer(state.player, switch throwing {
  | None => 1.
  | Some((timer, height)) => {
    let p =jumpPercent((timer, height));

    drawPlayerShadow(state.player, p, env);
    1. +. p
  }
  }, env);
  drawPower(throwTimer, env);

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

let flyIn = (state, prevPos, percent, env) => {
  let an = cos(percent *. 3.14159 /. 2.);
  let size = (1000. -. 80.) *. an +. 80.;
  drawLights(state, size, env);
  drawWalls(state.walls, purple, env);
  drawGoal(state.player.size, state.target, purple, env);
  drawPlayerShadow(state.player, an, env);
  drawPlayer(state.player, an +. 1., env);
  drawPower(state.throwTimer, env);
};

let flyOut = (state, nextState, percent, env) => {
  let an = 1. -. percent;
  let an = cos(an *. 3.14159 /. 2.);
  let size = (1000. -. 80.) *. an +. 80.;
  drawLights(state, size, env);
  let over = Geom.Ease.easeOutQuad(min(1., percent *. 2.));
  drawWalls(state.walls, withAlpha(purple, 1. -. over), env);
  drawWalls(nextState.walls, withAlpha(purple, over), env);
  drawGoal(state.player.size, state.target, withAlpha(purple, 1. -. over), env);
  drawGoal(nextState.player.size, nextState.target, withAlpha(purple, over), env);

  let player = {...state.player, pos: Geom.lerpPos(state.player.pos, nextState.player.pos, percent)};

  drawPlayerShadow(player, an, env);
  drawPlayer(player, an +. 1., env);
  drawPower(state.throwTimer, env);
};

let draw = ({status} as context, env) => {
  switch status {
  | Playing(state) => draw(state, context, env)
  | AnimateIn(prevState, state, timer) => {
      Draw.background(purple, env);
      switch prevState {
      | None => flyIn(state, None, Timer.percent(timer), env)
      | Some(prevState) => {
        switch (Timer.in2(timer)) {
        | `First(percent) => flyOut(prevState, state, percent, env)
        | `Second(percent) => flyIn(state, Some(prevState.player.pos), percent, env)
        }
      }
      }

    /* TODO prevState */

  }
  | _ => ()
  }
};