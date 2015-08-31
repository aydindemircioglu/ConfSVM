
muteFun = function(expr, mute = FALSE) {
  assertFlag(mute)
  if (mute) {
    BBmisc::suppressAll(expr)
  } else {
    expr
  }
}


