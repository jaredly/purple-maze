open Shared;
open Reprocessing;

let purple = Reprocessing.Utils.color(~r=255, ~g=0, ~b=255, ~a=255);
let darker = Reprocessing.Utils.color(~r=200, ~g=0, ~b=255, ~a=255);
let shadow = Reprocessing.Utils.color(~r=150, ~g=0, ~b=200, ~a=100);
let background = Utils.color(~r=255, ~g=220, ~b=255, ~a=255);
let pathColor = Utils.color(~r=255, ~g=240, ~b=255, ~a=255);
let pathColor = Utils.color(~r=255, ~g=100, ~b=255, ~a=255);
/* let pathColor = Utils.color(~r=245, ~g=0, ~b=255, ~a=255); */

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

let drawPlayerShadow = ({player: {pos: {Geom.x, y}}}, offset, env) => {
  Draw.noStroke(env);
  Draw.fill(shadow, env);
  let off = offset *. Shared.playerSize *. 5.;
  let size = Shared.playerSize;
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
  /* let light = 80.; */
  Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=light, ~rady=light, env);
};

let drawWalls = (state, env) => {
  Draw.strokeWeight(3, env);
  Draw.stroke(purple, env);
  List.iter(DrawMaze.draw_wall(env, (0., 0.)), state.walls);
};

let drawPath = (state, env) => {
  Draw.strokeWeight(10, env);
  Draw.stroke(pathColor, env);
  state.path |> Shared.LineSet.iter(((p1, p2)) => Draw.linef(~p1, ~p2, env));
};

let drawPlayer = ({player}, scale, env) => {
  Draw.noStroke(env);
  Draw.fill(darker, env);
  Draw.ellipsef(~center=Geom.tuple(player.pos), ~radx=Shared.playerSize *. scale, ~rady=Shared.playerSize *. scale, env);
};

let drawGoal = ({target}, env) => {
  Draw.stroke(background, env);
  Draw.strokeWeight(5, env);
  Draw.fill(purple, env);
  Draw.ellipsef(~center=target, ~radx=Shared.playerSize, ~rady=Shared.playerSize, env);
};

let draw = ({player, walls, target, throwTimer, throwing, path} as state, {textFont, width, height}, env) => {
  Draw.background(purple, env);
  drawLights(state, 80., env);
  drawJump(state, env);
  drawWalls(state, env);
  drawPath(state, env);
  drawPlayer(state, switch throwing {
  | None => 1.
  | Some((timer, height)) => {
    let p =jumpPercent((timer, height));

    drawPlayerShadow(state, p, env);
    1. +. p
  }
  }, env);
  drawGoal(state, env);
  drawPower(throwTimer, env);

  ()
};

let draw = ({status} as context, env) => {
  switch status {
  | Playing(state) => draw(state, context, env)
  | AnimateIn(prevState, state, timer) => {
      Draw.background(purple, env);
      let an = Timer.percent(timer);
      let an = cos(an *. 3.14159 /. 2.);
      let size = (1000. -. 80.) *. an +. 80.;
      drawLights(state, size, env);
      drawWalls(state, env);
      drawPlayerShadow(state, an, env);
      drawPlayer(state, an +. 1., env);
      drawGoal(state, env);
      drawPower(state.throwTimer, env);

    /* TODO prevState */

  }
  | _ => ()
  }
};