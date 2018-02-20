open Shared;
open Reprocessing;

let draw = ({textFont, user, walls, status, width, height}, env) => {
  Draw.background(Constants.white, env);
  Draw.stroke(Constants.red, env);
  List.iter(DrawMaze.draw_wall(env, (10., 10.)), walls);
  /* Draw.fill(Constants.red, env);
  let distance = getDistance(status);
  List.iter(
    ({x, y, w}) => {
      if (y +. distance +. 10. > 0. && y +. distance < height)
      Draw.rectf(~pos=(x, (y +. distance)), ~width=w, ~height=10., env)
    },
    platforms
  );
  let (x, y, _, _) = user;
  Draw.fill(Constants.blue, env);
  Draw.rectf(~pos=(x, (y +. distance)), ~width=10., ~height=10., env);
  Draw.tint(Constants.black, env);
  Draw.text(~font=textFont, ~pos=(10, 10), ~body=string_of_int(int_of_float(distance)), env);
  Draw.noTint(env); */
  ()
};