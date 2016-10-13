flipData = function(numFlips, theta = 0.5) {
  #Return coin flips, true is heads, false is tails
  return( runif(numFlips) < theta );
}

likelihood = function(flipSeq, thetaList) {
  return(thetaList^sum(flipSeq) * (1-thetaList)^(sum(!flipSeq)));
}

posterior = function(flipSeq, thetaList, priorDist) {
  #Calculate the likelihood of the sequence
  posterior = priorDist * likelihood(flipSeq, thetaList);
  posterior = posterior / sum(posterior);
  return(posterior);
}