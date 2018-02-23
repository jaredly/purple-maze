/** TODO this should move to belt or somewhere */
type t('a) = ((list('a), list('a)));
let empty = ([], []);
let isEmpty = x => x == empty;
let add = (x, (front, back)) => {
  ([x, ...front], back);
};
let create = x => add(x, empty);
let rec take = (queue) =>
  switch queue {
  | (front, [x]) => Some((x, ([], List.rev(front))))
  | (front, [x, ...back]) => Some((x, (front, back)))
  | ([], []) => None
  | (front, []) => take(([], List.rev(front)))
  };
let peek = ((front, back)) => {
  switch back {
  | [x, ...rest] => Some(x)
  | [] => {
    let l = List.length(front);
    if (l > 0) {
      Some(List.nth(front, l - 1))
    } else {
      None
    }
  }
  }
};
let dump = ((front, back)) => front @ back;
