ConfidenceLevelGH <- function(SE, confLevel = .90) {
  # Based on Goldstein & Healy, using Tommy's notation for z_alpha_a

  n <- length(SE)
  k <- matrix(NA, n, n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      k[i,j] <- (SE[i]+SE[j])/sqrt(SE[i]^2+SE[j]^2)
    }
  }
  mean_k <- mean(k, na.rm=TRUE)

  alpha <- qnorm(1 - (1 - confLevel) / 2)
  z_alpha_a <- alpha / mean_k
  alpha_a <- 2 * (1 - pnorm(z_alpha_a))
  conf.level_a <- 1 - alpha_a
  return(conf.level_a)
}
