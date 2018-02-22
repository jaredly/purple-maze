
module Ease = {
  let linear = t => t;
  /* // accelerating from zero velocity */
  let easeInQuad = t => t *. t;
  /* // decelerating to zero velocity */
  let easeOutQuad = t => t*. (2. -. t);
  /* // acceleration until halfway, then deceleration */
  let easeInOutQuad = (t) => t<0.5 ? 2. *. t*. t : -1. +. (4. -. 2. *. t)*. t;
  /* // accelerating from zero velocity  */
  /* easeInCubic: function (t) { return t*t*t },
  // decelerating to zero velocity
  easeOutCubic: function (t) { return (--t)*t*t+1 },
  // acceleration until halfway, then deceleration
  easeInOutCubic: function (t) { return t<.5 ? 4*t*t*t : (t-1)*(2*t-2)*(2*t-2)+1 },
  // accelerating from zero velocity
  easeInQuart: function (t) { return t*t*t*t },
  // decelerating to zero velocity
  easeOutQuart: function (t) { return 1-(--t)*t*t*t },
  // acceleration until halfway, then deceleration
  easeInOutQuart: function (t) { return t<.5 ? 8*t*t*t*t : 1-8*(--t)*t*t*t },
  // accelerating from zero velocity
  easeInQuint: function (t) { return t*t*t*t*t },
  // decelerating to zero velocity
  easeOutQuint: function (t) { return 1+(--t)*t*t*t*t },
  // acceleration until halfway, then deceleration
  easeInOutQuint: function (t) { return t<.5 ? 16*t*t*t*t*t : 1+16*(--t)*t*t*t*t } */
};

type point = {x: float, y: float};
type vector = {magnitude: float, theta: float};
let v0 = {magnitude: 0., theta: 0.};
type pector = {dx: float, dy: float};

let tuple = ({x, y}) => (x, y);
let dist = (p1, p2) => {
  let dx = p2.x -. p1.x;
  let dy = p2.y -. p1.y;
  sqrt(dx *. dx +. dy *. dy)
};

let scalePector = ({dx, dy}, scale) => {dx: dx *. scale, dy: dy *. scale};

let pdist = ({dx, dy}) => sqrt(dx *. dx +. dy *. dy);
let pdiff = (p1, p2) => {dx: p2.x -. p1.x, dy: p2.y -. p1.y};

let addVectorToPoint = ({magnitude, theta}, {x, y}) => {
  {x: x +. cos(theta) *. magnitude, y: y +. sin(theta) *. magnitude}
};
let vectorToPector = ({magnitude, theta}) => {dx: cos(theta) *. magnitude, dy: sin(theta) *. magnitude};
let pectorToVector = (p) => {
  magnitude: pdist(p),
  theta: atan2(p.dy, p.dx)
};
let addPectors = (p1, p2) => {dx: p1.dx +. p2.dx, dy: p1.dy +. p2.dy};
let clampVector = ({magnitude, theta}, maxMag) => {magnitude: min(maxMag, magnitude), theta};
let addVectors = (v1, v2) => addPectors(vectorToPector(v1), vectorToPector(v2)) |> pectorToVector;
let addPectorToVector = (p, v) => addPectors(p, vectorToPector(v)) |> pectorToVector;
let invertVector = ({magnitude, theta}) => {magnitude, theta: theta +. 3.14159};
let invertPector = ({dx, dy}) => {dx: -.dx, dy: -.dy};
let scaleVector = ({magnitude, theta}, scale) => {theta, magnitude: magnitude *. scale};

let lerpPos = (p1, p2, amount) => {
  let dx = p2.x -. p1.x;
  let dy = p2.y -. p1.y;
  {x: p1.x +. dx *. amount, y: p1.y +. dy *. amount}
};

module Circle = {
  type t = {rad: float, center: point};

  let testPoint = ({rad, center}, point) => dist(point, center) <= rad;
  let testCircle = (c1, c2) => dist(c1.center, c2.center) <= c1.rad +. c2.rad;
  /** based on http://www.jeffreythompson.org/collision-detection/poly-circle.php */
  let testLine = (c, p1, p2) => {
    testPoint(c, p1) ||
    testPoint(c, p2) || {
      let len = dist(p1, p2);
      let dot = (
        ((c.center.x -. p1.x)*.(p2.x -. p1.x)) +. ((c.center.y -. p1.y) *. (p2.y -. p1.y))
      ) /. (len *. len);

      let closestX = p1.x +. (dot *. (p2.x -. p1.x));
      let closestY = p1.y +. (dot *. (p2.y -. p1.y));

      let xa = min(p1.x, p2.x);
      let xb = max(p1.x, p2.x);
      let ya = min(p1.y, p2.y);
      let yb = max(p1.y, p2.y);

      /* tangent point is within the line segment */
      (xa == xb || (xa <= closestX && closestX <= xb)) &&
      (ya == yb || (ya <= closestY && closestY <= yb)) &&
      /* tangent point is within the circle */
      testPoint(c, {x: closestX, y: closestY})
    }
  };

  let vectorToLine = (c, p1, p2) => {
    let len = dist(p1, p2);
    let dot = (
      ((c.center.x -. p1.x)*.(p2.x -. p1.x)) +. ((c.center.y -. p1.y) *. (p2.y -. p1.y))
    ) /. (len *. len);

    let closestX = p1.x +. (dot *. (p2.x -. p1.x));
    let closestY = p1.y +. (dot *. (p2.y -. p1.y));

    let xa = min(p1.x, p2.x);
    let xb = max(p1.x, p2.x);
    let ya = min(p1.y, p2.y);
    let yb = max(p1.y, p2.y);

    /* tangent point is within the line segment */
    if ((xa == xb || (xa <= closestX && closestX <= xb)) &&
        (ya == yb || (ya <= closestY && closestY <= yb))) {
      {
        dx: closestX -. c.center.x,
        dy: closestY -. c.center.y
      }
    } else {
      let p1diff = pdiff(c.center, p1);
      let p2diff = pdiff(c.center, p2);
      let d1 = pdist(p1diff);
      let d2 = pdist(p2diff);
      d1 < d2 ? p1diff : p2diff
    }
  };
};

let pi = 3.14159;
let tau = pi *. 2.;

let clockwiseAngleDiff = (first, second) => {
  let first = mod_float(first, tau);
  let second = mod_float(first, tau);
  let diff = second -. first;
  if (diff < 0.) {
    diff +. tau
  } else {
    diff
  }
  /* if (first > second) {
    second -. mod_float(first, tau)
  } else {
    mod_float((second -. first), tau)
  } */
};

[@test [
  /* (0., 1., 0.5),
  (-0.1, 0.1, 0.0),
  (pi -. 0.1, -. pi +. 0.1, pi),
  (pi -. 0.1, -. pi +. 0.1, -. pi), */
  (tau -. 0.1, tau, -0.05),
  (pi, pi +. 0.1, -. pi +. 0.05)
]]
let isThetaBetween = (low, high, test) => {
  let a = clockwiseAngleDiff(low, test);
  a >= 0. && a <= clockwiseAngleDiff(low, high)
};

let rec normalize = x => {
  if (x < -. pi) normalize(x +. tau)
  else if (x > pi) normalize(x -. tau)
  else x
};

let isThetaBetween = (low, high, test) => {
  let low = low -. test |> normalize;
  let high = high -. test |> normalize;
  if (low *. high >= 0.) {
    false
  } else {
    abs_float(high -. low) < pi
  }
};

module Arc = {
  type t = {cx: float, cy: float, r: float, t1: float, t2: float};
  let points = ({cx, cy, r, t1, t2}) => {
    (
      {x: cos(t1) *. r +. cx, y: sin(t1) *. r +. cy},
      {x: cos(t2) *. r +. cx, y: sin(t2) *. r +. cy},
    )
  };

  let testCircle = ({cx, cy, r, t1, t2} as arc, c) => {
    let t2 = (t1 > t2) ? t2 +. tau : t2;
    let (p1, p2) = points(arc);
    let v = pectorToVector({dx: c.Circle.center.x -. cx, dy: c.Circle.center.y -. cy});
    if (isThetaBetween(t1, t2, v.theta)) {
    /* if (t1 <= v.theta && v.theta <= t2) { */
      v.magnitude < r +. c.Circle.rad &&
      v.magnitude > r -. c.Circle.rad
    } else {
      Circle.testPoint(c, p1) || Circle.testPoint(c, p2)
    }
  };

  let vectorToCircle = ({cx, cy, r, t1, t2} as arc, c) => {
    /* let t1 = (t1 > t2) ? t1 -. tau : t1; */
    let t2 = (t1 > t2) ? t2 +. tau : t2;
    let (p1, p2) = points(arc);
    let v = pectorToVector({dx: c.Circle.center.x -. cx, dy: c.Circle.center.y -. cy});
    if (isThetaBetween(t1, t2, v.theta)) {
    /* if (t1 <= v.theta && v.theta <= t2) { */
      vectorToPector({magnitude: v.magnitude -. r, theta: v.theta})
    } else {
      let p1diff = pdiff(c.Circle.center, p1);
      let p2diff = pdiff(c.Circle.center, p2);
      let d1 = pdist(p1diff);
      let d2 = pdist(p2diff);
      invertPector(d1 < d2 ? p1diff : p2diff)
    }
  };
};

module Aabb = {
  type t = {x0: float, y0: float, x1: float, y1: float};
  let testPoint = ({x0, y0, x1, y1}, {x, y}) => {
    x0 <= x && x <= x1 &&
    y0 <= y && y <= y1
  };

  let fromPoint = ({x, y}) => {x0: x, y0: y, x1: x, y1: y};

  let fromPoints = points => {
    Array.fold_left(
      ({x0, y0, x1, y1}, {x, y}) => (
        {x0: min(x0, x), y0: min(y0, y), x1: max(x1, x), y1: max(y1, y)}
      ),
      fromPoint(points[0]),
      points
    )
  };
};

module Polygon = {
  type t = {
    aabb: Aabb.t,
    vertices: array(point),
  };

  let fromVertices = vertices => {vertices, aabb: Aabb.fromPoints(vertices)};

  let verticesToPoint = (vertices, point) => {
    let px = point.x;
    let py = point.y;
    Array.fold_left(
      ((inside, prev), current) => {
        let vc = prev;
        let vn = current;
        if (
          ((vc.y > py && vn.y < py) || (vc.y < py && vn.y > py)) &&
          (px < (vn.x -. vc.x)*.(py -. vc.y) /. (vn.y -. vc.y) +. vc.x)
        ) {
          (!inside, current)
        } else {
          (inside, current)
        }
      },
      (false, vertices[Array.length(vertices) - 1]),
      vertices
    ) |> fst
  };

  let testPoint = ({aabb, vertices}, point) => {
    Aabb.testPoint(aabb, point) &&
    verticesToPoint(vertices, point)
  };

  let testCircle = ({aabb, vertices}, {Circle.rad, center} as c) => {
    Aabb.testPoint(aabb, center) &&
    (
      /** TODO benchmark which of these should go first */
      verticesToPoint(vertices, center)
      ||
      {
        let len = Array.length(vertices);
        let rec loop = (i) => {
          let pi = i == 0 ? len - 1 : i - 1;

          Circle.testLine(c, vertices[pi], vertices[i]) ||
          (i == len - 1 ? false : loop(i + 1))
        };
        loop(0);
      }
    )
  };
};


/* boolean polyPoint(PVector[] vertices, float px, float py) {
  boolean collision = false;

  // go through each of the vertices, plus
  // the next vertex in the list
  int next = 0;
  for (int current=0; current<vertices.length; current++) {

    // get next vertex in list
    // if we've hit the end, wrap around to 0
    next = current+1;
    if (next == vertices.length) next = 0;

    // get the PVectors at our current position
    // this makes our if statement a little cleaner
    PVector vc = vertices[current];    // c for "current"
    PVector vn = vertices[next];       // n for "next"

    // compare position, flip 'collision' variable
    // back and forth
    if (((vc.y > py && vn.y < py) || (vc.y < py && vn.y > py)) &&
         (px < (vn.x-vc.x)*(py-vc.y) / (vn.y-vc.y)+vc.x)) {
            collision = !collision;
    }
  }
  return collision;
} */
