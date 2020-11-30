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
  n <- length(unlist(y))
  G <- matrix(NA, nrow = nrow(X), ncol = ncol(X))
  for (i in 1:(m - 1)) {
    G[, i] <- box_cox_trans(unlist(X[, ..i]), lambda[i])
  }
  D <- cbind(rep(1, n), G)
  A <- diag(n) - (D %*% solve(t(D) %*% D) %*% t(D))
  g_gamma <- box_cox_trans(unlist(y), lambda[m])
  quad_pars <- t(g_gamma) %*% A %*% g_gamma
  const <- -n * log(2 * pi * exp(1) / n) / 2
  log_like <- const - n * log(quad_pars) / 2 + (lambda[m] - 1) * sum(log(y))
  return(log_like)
}

# optimizing the parameters
# using initial condition c(1,1,0,1,0,0,0,1,-1)
init_lambda <- c(1, 1, 0, 1, 0, 0, 0, 1)
init_theta <-  -1

mle_opt <- optim(
  par = c(init_lambda, init_theta),
  fn = mle_pars,
  y = senic[, "Stay"],
  X = senic[, -"Stay"],
  control = list(fnscale = -1, maxit = 100)
)

best_lambda <- mle_opt$par[1:8]
best_theta <- mle_opt$par[9]

sugg_lambda <- c(0, 2, 1, 1, 0, 0, 1, -1)
sugg_theta <- -1/2

init_loglik <- mle_pars(c(init_lambda, init_theta), senic[, "Stay"], senic[, -"Stay"])[1, 1]
sugg_loglik <- mle_pars(c(sugg_lambda, sugg_theta), senic[, "Stay"], senic[, -"Stay"])[1, 1]
mle_loglik <- mle_opt$value
mle_loglik > init_loglik



# part b ------------------------------------------------------------------



# we are going to roll with initial guess, even though its not the best
senic_init <- senic[
  , c(
    "w", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8"
  ) := .(
    power_trans(Stay,   init_theta),
    power_trans(Age,    init_lambda[1]),
    power_trans(Risk,   init_lambda[2]),
    power_trans(Cult,   init_lambda[3]),
    power_trans(Xray,   init_lambda[4]),
    power_trans(Beds,   init_lambda[5]),
    power_trans(Cen,    init_lambda[6]),
    power_trans(Nurses, init_lambda[7]),
    power_trans(AFS,    init_lambda[8])
  )
][
  , c("w", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
]

# making correlation matrix
GGally::ggpairs(senic_init) + 
  theme_bw(base_size = 10)
ggsave("hwk/hwk05/img/q02-correlation-matrix.png")




# part c and d ------------------------------------------------------------



# using forward stepwise regression
fwd_model <- leaps::regsubsets(w ~ ., data = senic_init, method = "forward")
fwd_summary <- summary(fwd_model)

# using backward stepwise regression
bwd_model <- leaps::regsubsets(w ~ ., data = senic_init, method = "backward")
bwd_summary <- summary(fwd_model)

# function for finding best model for various criteria
best_mod <- function(method = "forward", criteria = "Cp") {
  ###
  if (method == "forward") {
    ##
    if (criteria == "Cp") {
      val <- fwd_summary$cp[1]
      best_num <- 1
      for (i in 2:8) {
        if (fwd_summary$cp[i] < val) {
          val <- fwd_summary$cp[i]
          best_num <- i
        }
      }
      best_coef <- fwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
    ##
    if (criteria == "BIC") {
      val <- fwd_summary$bic[1]
      best_num <- 1
      for (i in 2:8) {
        if (fwd_summary$bic[i] < val) {
          val <- fwd_summary$bic[i]
          best_num <- i
        }
      }
      best_coef <- fwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
    ##
    if (criteria == "adjR2") {
      val <- fwd_summary$adjr2[1]
      best_num <- 1
      for (i in 2:8) {
        if (fwd_summary$adjr2[i] > val) {
          val <- fwd_summary$adjr2[i]
          best_num <- i
        }
      }
      best_coef <- fwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
  }
  ###
  if (method == "backward") {
    ##
    if (criteria == "Cp") {
      val <- bwd_summary$cp[1]
      best_num <- 1
      for (i in 2:8) {
        if (bwd_summary$cp[i] < val) {
          val <- bwd_summary$cp[i]
          best_num <- i
        }
      }
      best_coef <- bwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
    ##
    if (criteria == "BIC") {
      val <- bwd_summary$bic[1]
      best_num <- 1
      for (i in 2:8) {
        if (bwd_summary$bic[i] < val) {
          val <- bwd_summary$bic[i]
          best_num <- i
        }
      }
      best_coef <- bwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
    ##
    if (criteria == "adjR2") {
      val <- bwd_summary$adjr2[1]
      best_num <- 1
      for (i in 2:8) {
        if (bwd_summary$adjr2[i] > val) {
          val <- bwd_summary$adjr2[i]
          best_num <- i
        }
      }
      best_coef <- bwd_summary$which[best_num, ]
      best_coef_names <- names(best_coef)[which(best_coef == TRUE)]
    }
  }
  return(
    list(numb_coef = best_num, best_coef = best_coef_names[-1])
  )
}

# checking to see if models are the same
cp_fwd_coef <- best_mod(method = "forward",  criteria = "Cp")[[2]]
cp_bwd_coef <- best_mod(method = "backward", criteria = "Cp")[[2]]
all.equal(cp_fwd_coef, cp_bwd_coef)

bic_fwd_coef <- best_mod(method = "forward",  criteria = "BIC")[[2]]
bic_bwd_coef <- best_mod(method = "backward", criteria = "BIC")[[2]]
all.equal(bic_fwd_coef, bic_bwd_coef)

adjr2_fwd_coef <- best_mod(method = "forward",  criteria = "adjR2")[[2]]
adjr2_bwd_coef <- best_mod(method = "backward", criteria = "adjR2")[[2]]
all.equal(adjr2_fwd_coef, adjr2_bwd_coef)
