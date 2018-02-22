
type t = (float, float);
let percent = ((amount, full)) => amount /. full;

let inc = ((amount, full), env) => (min(full, amount +. Reprocessing.Env.deltaTime(env)), full);
let isFull = ((amount, full)) => amount == full;
let restart = ((_, full)) => (0., full);
let incLoop = (timer, env) => {
  let timer = inc(timer, env);
  (isFull(timer) ? restart(timer) : timer, isFull(timer))
};
let fill = ((_, full)) => (full, full);
let createFull = full => (full, full);
let createEmpty = full => (0., full);

let in2 = ((amount, full)) => {
  let p = amount /. full;
  if (p < 0.5) {
    `First(p *. 2.)
  } else {
    `Second((p -. 0.5) *. 2.)
  }
};

let in3 = ((amount, full)) => {
  let p = amount /. full;
  if (p < 1. /. 3.) {
    `First(p *. 3.)
  } else if (p < 2. /. 3.) {
    `Second((p -. 1. /. 3.) *. 3.)
  } else {
    `Third((p -. 2. /. 3.) *. 3.)
  }
};