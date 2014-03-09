var myRotate = function (xs) { return xs.slice(1).concat(xs[0]) }
var myRotateN = function (n, xs) {
  return (n < 1) ? xs : myRotateN(n-1, myRotate(xs))
}
