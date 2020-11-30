myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mydblue   <- "#0080ff"
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
)[, .(Nurses, AFS)]

# histogram and histograms for nurses
ggplot(senic, aes(x = Nurses, y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myblue) +
  labs(x = "Nurses", y = "Density") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-nurses-hist-original.png")

ggplot(senic, aes(y = Nurses)) +
  geom_boxplot(fill = myblue) +
  labs(x = "", y = "Nurses") +
  theme_bw(base_size = 30)

# histogram and scatterplot for nurses
ggplot(senic, aes(x = AFS, y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myorange) +
  labs(x = "AFS", y = "Density") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-afs-hist-original.png")

ggplot(senic, aes(y = AFS)) +
  geom_boxplot(fill = myorange) +
  labs(x = "", y = "AFS") +
  theme_bw(base_size = 30)

# making scatterplot
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(color = dark1, cex = 4, pch = 1, stroke = 2) +
  labs(x = "AFS", y = "Nurses") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-scatterplot-blank.png")

# general observations: 
# - Nurses could be gamma distributed
# - AFS looks normally distributed
# - looking at scatterplot, maybe log transform?
# - log transform looks good, it makes histogram more normal and scatterplot looks linear

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

# function for box-cox transform, and inverse
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

# function for log likelihood value depending on lambda
mle_lambda <- function(lambda, y, X) {
  m        <- length(lambda)
  n        <- length(y)
  D        <- cbind(rep(1, n), X)
  A        <- diag(n) - (D %*% solve(t(D) %*% D) %*% t(D))
  log_like <- rep(NA, m)
  for (i in 1:m) {
    g_lambda    <- box_cox_trans(y, lambda[i])
    quad_lambda <- t(g_lambda) %*% A %*% g_lambda
    const       <- -n * log(2 * pi * exp(1) / n) / 2
    log_like[i] <- const - n * log(quad_lambda) / 2 + (lambda[i] - 1) * sum(log(y))
  }
  return(log_like)
}

# finding the best lambda
best_pair <- data.table(
  lambda = seq(-1, 1, 0.001)
)[, log_like := mle_lambda(lambda, senic$Nurses, senic$AFS)
  ][, .(lambda_mle = lambda[which.max(log_like)], max_log_like = max(log_like))]

best_lambda <- best_pair$lambda_mle

# NOTE
# Now that we have the best labda, we are working with flambda, not glambda
# glamnda is used in theoretical derivations

# adding transformed variables to the data
# senic[
#   , c("power_Nurses", "box_cox_Nurses") := .(
#     power_trans(Nurses, best_lambda), box_cox_trans(Nurses, best_lambda)
#   )
# ]
senic[
  , "power_Nurses" := power_trans(Nurses, best_lambda)
]

# making plot of log-likelihood
# this works now bc I vectorized function!
ggplot(data.table(x = seq(-1, 1, 0.001)), aes(x)) +
  geom_function(fun = mle_lambda, args = list(y = senic$Nurses, X = senic$AFS), lwd = 1.5) +
  geom_vline(xintercept = best_pair$lambda_mle, color = myred, linetype = "dashed", lwd = 1.5) +
  geom_point(data = best_pair, mapping = aes(x = lambda_mle, y = max_log_like), color = myred, cex = 5) +
  labs(x = expression(lambda), y = expression(m(lambda))) +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-box-cox-lambda.png")

# making histogram with most optimal transformed data
# ggplot(senic, aes(x = box_cox_Nurses, y = ..density..)) +
#   geom_histogram(bins = 20, boundary = 0, fill = myblue) +
#   labs(x = expression(g[0.085](Nurses)), y = "Density") +
#   theme_bw(base_size = 30)
# ggsave("hwk/hwk05/img/q01-nurses-hist-transformed.png")

# making it for original power transformation
ggplot(senic, aes(x = power_Nurses, y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myblue) +
  labs(x = expression(f[0.085](Nurses)), y = "Density") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-nurses-hist-transformed.png")



# part c ------------------------------------------------------------------



# creating function of y (i.e. transforming box_cox_trans(Y) back to Y)
power_fit <- lm(power_Nurses ~ AFS, data = senic)

# box_cox_fit <- lm(box_cox_Nurses ~ AFS, data = senic)

# adding fitted values and re-transformed values for both power and box_cox
# senic[
#   , c("power_fitted", "box_cox_fitted") := .(
#     power_fit$fitted.values, box_cox_fit$fitted.values
#   )
# ][
#   , c("Nurses_fit_power", "Nurses_fit_box_cox") := .(
#     power_inv(power_fitted, best_lambda), box_cox_inv(box_cox_fitted, best_lambda)
#   )
# ]
senic[
  , "power_Nurses_fitted" := power_fit$fitted.values
][
  , "Nurses_fitted" := power_inv(power_Nurses_fitted, best_lambda)
]


power_inv_fit <- function(y, lambda = best_lambda) {
  fit <- lm(power_Nurses ~ AFS, data = senic)
  coef <- fit$coefficients
  power_inv(coef[1] + coef[2] * y, lambda)
}

# box_cox_inv_fit <- function(y, lambda = best_lambda) {
#   fit <- lm(power_Nurses ~ AFS, data = senic)
#   coef <- fit$coefficients
#   box_cox_inv(coef[1] + coef[2] * y, lambda)
# }

# making function for confidence intervals so it can be added to senic
power_confint <- function(y) {
  m <- length(y)
  n <- length(senic$power_Nurses)
  xbar <- mean(senic$AFS)
  ssx <- sum((senic$AFS - xbar)^2)
  shat <- sqrt(sum((senic$power_Nurses - senic$power_Nurses_fitted)^2))
  yhat <- predict(power_fit, data.table(AFS = y))
  lwr_upr <- list("lwr", "upr")
  for (i in 1:m) {
    lwr_upr$lwr[i] <- yhat[i] - qt(0.975, n - 2) * shat * sqrt(1 / n + (y[i] - xbar)^2 / ssx)
    lwr_upr$upr[i] <- yhat[i] + qt(0.975, n - 2) * shat * sqrt(1 / n + (y[i] - xbar)^2 / ssx)
  }
  return(lwr_upr)
}

# adding points to senic
senic[
  , c("power_Nurses_ci_lwr", "power_Nurses_ci_upr") := .(
    power_confint(AFS)$lwr, power_confint(AFS)$upr 
  )
][
  , c("Nurses_ci_lwr", "Nurses_ci_upr") := .(
    power_inv(power_Nurses_ci_lwr, best_lambda), power_inv(power_Nurses_ci_upr, best_lambda)
  )
]

# making plot for confidence interval
ggplot(senic, aes(AFS, power_Nurses)) +
  geom_point(color = dark1, cex = 4, pch = 1, stroke = 2) +
  geom_ribbon(aes(ymin = power_Nurses_ci_lwr, ymax = power_Nurses_ci_upr), fill = myred, alpha = 0.3) +
  geom_line(aes(AFS, power_Nurses_fitted), color = myred, cex = 2) +
  labs(x = "AFS", y = expression(f[0.085](Nurses))) +
  theme_bw(base_size = 30)


ggplot(senic, aes(AFS, Nurses)) +
  geom_point(color = dark1, cex = 4, pch = 1, stroke = 2) +
  geom_ribbon(aes(ymin = Nurses_ci_lwr, ymax = Nurses_ci_upr), fill = myred, alpha = 0.3) +
  geom_line(aes(AFS, Nurses_fitted), color = myred, cex = 2) +
  labs(x = "AFS", y = "Nurses") +
  coord_cartesian(ylim = c(14, 656)) +
  theme_bw(base_size = 30)

