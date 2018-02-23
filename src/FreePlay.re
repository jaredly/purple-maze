
type t = (int, Play_types.status);

let init = (size, context, env) => {
  (size, Play_step.start(size, env))
};

let step = ((size, status), context, env) => {
  if (Reprocessing.Env.keyPressed(Reprocessing.Events.Escape, env)) {
    init(size, context, env)
  } else {
    switch (Play_step.step(status, context, env)) {
    | `Continue(status) => (size, status)
    | `Won(prevState) => (size, Play_step.continue(prevState, size, env))
    }
  }
};

let draw = ((size, status), context, env) => Play_draw.draw(status, context, env);