open Shared;
open Reprocessing;

let purple = Reprocessing.Utils.color(~r=255, ~g=0, ~b=255, ~a=255);
let darker = Reprocessing.Utils.color(~r=200, ~g=0, ~b=255, ~a=255);
let background = Utils.color(~r=255, ~g=220, ~b=255, ~a=255);

Printexc.record_backtrace(true);

let draw = ({player, walls, target, throwTimer, throwing}, {textFont, width, height}, env) => {
  Draw.background(purple, env);
  Draw.strokeWeight(3, env);

  /** your personal light */
  /* Draw.fill(Constants.white, env); */
  Draw.fill(background, env);
  let light = 80.;
  Draw.ellipsef(~center=(player.x, player.y), ~radx=light, ~rady=light, env);

  /** Thrown light */
  switch throwing {
  | None => ()
  | Some((timer, height)) => {
    let percent = Timer.percent(timer);
    let p = sqrt(sin(percent *. 3.14159));
    let top = 140. +. 200. *. height;
    /* let percent = 1.; */
    let full = top *. p;

    Draw.fill(Utils.color(~r=255, ~g=200, ~b=255, ~a=255), env);
    let size = full;
     /* *. percent; */
    Draw.ellipsef(~center=(player.x, player.y), ~radx=size, ~rady=size, env);
  }
  };

  /* Now the walls */
  Draw.stroke(purple, env);
  List.iter(DrawMaze.draw_wall(env, (0., 0.)), walls);

  /* Now you */
  Draw.noStroke(env);
  Draw.fill(darker, env);
  Draw.ellipsef(~center=(player.x, player.y), ~radx=Shared.playerSize, ~rady=Shared.playerSize, env);

  /* The target */
  Draw.stroke(background, env);
  Draw.strokeWeight(5, env);
  Draw.fill(purple, env);
  Draw.ellipsef(~center=target, ~radx=Shared.playerSize, ~rady=Shared.playerSize, env);


  Draw.strokeWeight(3, env);
  Draw.noFill(env);
  Draw.stroke(Constants.black, env);
  Draw.rectf(~pos=(10., 10.), ~width=20., ~height=100., env);

  Draw.noStroke(env);
  Draw.fill(Constants.black, env);
  let height = 100. *. Timer.percent(throwTimer);
  Draw.rectf(~pos=(10., 110. -. height), ~width=20., ~height=height, env);
  ()
};

let draw = ({status} as context, env) => {
  switch status {
  | Playing(state) => draw(state, context, env)
  | _ => ()
  }
};