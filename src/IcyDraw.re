open Shared;
open Reprocessing;

let draw = ({textFont, player, walls, target, status, width, height}, env) => {
  Draw.background(Constants.white, env);
  Draw.stroke(Constants.red, env);
  List.iter(DrawMaze.draw_wall(env, (0., 0.)), walls);
  Draw.fill(Constants.red, env);
  Draw.ellipsef(~center=(player.x, player.y), ~radx=10., ~rady=10., env);
  Draw.fill(Constants.black, env);
  Draw.noStroke(env);
  Draw.ellipsef(~center=target, ~radx=10., ~rady=10., env);
  ()
};