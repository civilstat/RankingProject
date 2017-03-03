range2units = function(extrange) {
  exp5 = floor(log10(extrange/2.5))
  if (floor(log2(extrange/(2.5*10^exp5))) == 0) {
    exp2 = exp5 - 1
  } else if (floor(log2(extrange/(2.5*10^exp5))) == 1) {
    exp2 = exp5
  } else {
    exp2 = exp5 + 1
  }
  unit = 2^exp2*5^exp5
  return(unit)
}
