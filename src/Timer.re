
type t = (float, float);
let percent = ((amount, full)) => amount /. full;

let inc = ((amount, full), env) => (min(full, amount +. Reprocessing.Env.deltaTime(env)), full);
let isFull = ((amount, full)) => amount == full;
let restart = ((_, full)) => (0., full);
let fill = ((_, full)) => (full, full);
let createFull = full => (full, full);
let createEmpty = full => (0., full);