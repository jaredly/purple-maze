open Shared;
open Reprocessing;

let draw = ({textFont, player, walls, status, width, height}, env) => {
  Draw.background(Constants.white, env);
  Draw.stroke(Constants.red, env);
  List.iter(DrawMaze.draw_wall(env, (10., 10.)), walls);
  Draw.fill(Constants.red, env);
  Draw.ellipsef(~center=(player.x +. 10., player.y +. 10.), ~radx=10., ~rady=10., env);
  ()
};