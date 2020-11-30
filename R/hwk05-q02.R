myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mypink   <- "#ff99ff"
dark1   <- "#565657"
dark2   <- "#808081"
dark3   <- "#aaaaab"
library(data.table)
library(ggplot2)


# part a ------------------------------------------------------------------


# reading in data for nurses and AFS
senic <- fread(
  input = "data/SENIC.txt"
)[, .(Age, Risk, Cult, Xray, Beds, Cen, Nurses, AFS, Stay)]

# functions for power transformation and inverse
power_trans <- function(y, lambda = 0) {
  n <- length(y)
  lambda_vec <- rep(lambda, n)
  vec <- rep(NA, n)
  for (i in 1:n) {
    if (lambda == 0) {
      vec[i] <- log(y[i])
    } else {
      vec[i] <- y[i]^lambda
    }
  }
  return(vec)
}

power_inv <- function(y, lambda = 0) {
  n <- length(y)
  lambda_vec <- rep(lambda, n)
  vec <- rep(NA, n)
  for (i in 1:n) {
    if (lambda == 0) {
      vec[i] <- exp(y[i])
    } else {
      vec[i] <- y[i]^(1 / lambda)
    }
  }
  return(vec)
}

# function for box-cox transformation and inverse
box_cox_trans <- function(y, lambda = 0) {
  n <- length(y)
  lambda_vec <- rep(lambda, n)
  vec <- rep(NA, n)
  for (i in 1:n) {
    if (lambda == 0) {
      vec[i] <- log(y[i])
    } else {
      vec[i] <- (y[i]^lambda - 1) / lambda
    }
  }
  return(vec)
}

box_cox_inv <- function(y, lambda = 0) {
  n <- length(y)
  lambda_vec <- rep(lambda, n)
  vec <- rep(NA, n)
  for (i in 1:n) {
    if (lambda == 0) {
      vec[i] <- exp(y[i])
    } else {
      vec[i] <- (1 + lambda * y[i])^(1 / lambda)
    }
  }
  return(vec)
}

# function for log likelihood value depending on gamma and lambda
# we are not trying to vectorize this anymore (as required by optim function)
mle_pars <- function(lambda, y, X) {
  m <- length(lambda)
  n <- length(y)
  G <- X
  for (i in 1:(m - 1)) {
    G[, i] <- box_cox_trans(unlist(X[, i]), lambda[i])
  }
  D <- cbind(rep(1, n), G)
  A <- diag(n) - (D %*% solve(t(D) %*% D) %*% t(D))
  g_gamma <- box_cox_trans(y, lambda[m])
  quad_pars <- t(g_gamma) %*% A %*% g_gamma
  const <- -n * log(2 * pi * exp(1) / n) / 2
  log_like <- const - n * log(quad_pars) / 2 + (lambda[m] - 1) * sum(log(y))
  return(log_like)
}

# optimizing the parameters
# using initial condition c(1,1,0,1,0,0,0,1,-1)
optim(
  par = c(1, 1, 0, 1, 0, 0, 0, 1, -1),
  fn = mle_pars,
  y = senic[, "Stay"],
  X = senic[, -"Stay"]
)
