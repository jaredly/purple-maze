
type t = (int, int, Play_types.status);

let init = (size, context, env) => {
  (size, 0, Play_step.start(size, env))
};

let step = ((size, num, status), context, env) => {
  if (Reprocessing.Env.keyPressed(Reprocessing.Events.Escape, env)) {
    init(size, context, env)
  } else {
    switch (Play_step.step(status, context, env)) {
    | `Continue(status) => (size, num, status)
    | `Won(prevState) => {
      let size = num >= 4 ? size + 1 : size;
      (size, num >= 4 ? 0 : num + 1, Play_step.continue(prevState, size, env))
    }
    }
  }
};

let draw = ((size, num, status), context, env) => {
  Play_draw.draw(status, context, env);

  Reprocessing.Draw.tint(Play_draw.textColor, env);
  Reprocessing.Draw.text(~font=context.textFont, ~body=Printf.sprintf("%d.%d", size, num + 1), ~pos=(Reprocessing.Env.width(env) - 50, 10), env);

  Reprocessing.Draw.text(~font=context.smallFont, ~body="PurpleMaze - by Jared Forsyth", ~pos=(10, int_of_float(context.height) - 60), env);
  Reprocessing.Draw.text(~font=context.smallFont, ~body="Made with ReasonML and Reprocessing", ~pos=(10, int_of_float(context.height) - 30), env);
  Reprocessing.Draw.noTint(env);
};