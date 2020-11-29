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

# question 1 --------------------------------------------------------------

senic <- fread(input = "data/SENIC.txt")

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
  if (lambda == 0) {
    log(y)
  } else {
    y^lambda
  }
}

power_inv <- function(y, lambda = 0) {
  if (lambda == 0) {
    exp(y)
  } else {
    y^(1 / lambda)
  }
}

# function for box-cox transform, and inverse
box_cox_trans <- function(y, lambda = 0) {
  if (lambda == 0) {
    log(y)
  } else {
    (y^lambda - 1) / lambda
  }
}

box_cox_inv <- function(y, lambda = 0) {
  if (lambda == 0) {
    exp(y)
  } else {
    (1 + lambda * y)^(1 / lambda)
  }
}

# function for log likelihood value depending on lambda
# originally called max_mle
mle_lambda <- function(lambda, y, X) {
  n           <- length(y)
  D           <- cbind(rep(1, length(y)), X)
  A           <- diag(n) - (D %*% solve(t(D) %*% D) %*% t(D))
  g_lambda    <- box_cox_trans(y, lambda)
  quad_lambda <- t(g_lambda) %*% A %*% g_lambda
  const       <- -n * log(2 * pi * exp(1) / n) / 2
  log_like    <- const - n * log(quad_lambda) / 2 + (lambda - 1) * sum(log(y))
  return(log_like)
}

# finding the best lambda
try_lambda <- seq(-1, 1, 0.005)
max_mle_vals <- rep(NA, length(try_lambda))
for (i in 1:length(try_lambda)) {
  max_mle_vals[i] <- max_mle(try_lambda[i], senic$Nurses, senic$Age)
}
lambda_mles <- data.table(lambda = try_lambda, loglikelihood = max_mle_vals)
(best_lamb <- lambda_mles[which.max(lambda_mles$loglikelihood)])

# making the plot
ggplot(lambda_mles, aes(x = lambda, y = loglikelihood)) +
  geom_point(cex = 1) +
  geom_vline(xintercept = best_lamb$lambda, color = myred, linetype = "dashed", lwd = 1.5) +
  geom_point(data = best_lamb, mapping = aes(x = lambda, y = loglikelihood), color = myred, cex = 5) +
  labs(x = expression(lambda), y = expression(m(lambda))) +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-box-cox-lambda.png")

# tried this but didn't work, will probably have to write function better
# ggplot(data.table(x = seq(-2, 2, 0.01)), aes(x)) +
#   geom_function(fun = max_mle, args = list(y = senic$Nurses, X = senic$AFS))

# making histogram with most optimal transformed data
ggplot(senic, aes(x = box_cox_trans(Nurses, lambda = best_lamb$lambda), y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myblue) +
  labs(x = expression(g[0.09](Nurses)), y = "Density") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-nurses-hist-transformed.png")

# creating function of y (i.e. transforming box_cox_trans(Y) back to Y)
power_fit <- lm(box_cox_trans(Nurses, lambda = best_lamb$lambda) ~ AFS, data = senic)
transform_fit <- function(x) {
  l <- best_lamb$lambda
  coef <- power_fit$coefficients
  box_cox_inv(
    coef[1] + coef[2] * x, 
    lambda = l
    )
}

# creating functions for confidence interval lower and upper bounds
confint_lwr <- function(x) {
  n <- length(senic$Nurses)
  l <- best_lamb$lambda
  coef <- power_fit$coefficients
  glambda <- box_cox_trans(senic$Nurses, lambda = best_lamb$lambda)
  slambda <- sum((glambda - power_fit$fitted.values)^2) / (n - 2)
  tstat <- qt(p = 0.975, df = n - 2)
  lwr_val <- box_cox_inv(
    coef[1] + coef[2] * x - (tstat * sqrt(slambda / n)), 
    lambda = best_lamb$lambda
    )
  return(lwr_val)
}

confint_upr <- function(x) {
  n <- length(senic$Nurses)
  l <- best_lamb$lambda
  coef <- power_fit$coefficients
  glambda <- box_cox_trans(senic$Nurses, lambda = best_lamb$lambda)
  slambda <- sum((glambda - power_fit$fitted.values)^2) / (n - 2)
  tstat <- qt(p = 0.975, df = n - 2)
  upr_val <- box_cox_inv(
    coef[1] + coef[2] * x + (tstat * sqrt(slambda / n)), 
    lambda = best_lamb$lambda
  )
  return(upr_val)
}

senic_ci <- senic[, .(AFS, Nurses)
                  # ][, "trans_Nurses" := box_cox_trans(Nurses, best_lamb$lambda)
                  ][, "fitted_Nurses" := box_cox_inv(power_fit$fitted.values, best_lamb$lambda)
                    ][, c("ci_lwr", "ci_upr") := list(confint_lwr(fitted_Nurses), confint_upr(fitted_Nurses))][]

# making scatterplot with optimal lambda function fit over it
# want to add confidence intervals to this
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(color = dark1, cex = 4, pch = 1, stroke = 2) +
  geom_function(fun = confint_lwr, color = myred, lwd = 1) +
  geom_function(fun = confint_upr, color = myred, lwd = 1) +
  # geom_ribbon(aes(ymin = ci_lwr, ymax = ci_upr)) +
  geom_function(fun = transform_fit, color = myred, lwd = 3) +
  # geom_smooth(method = "lm") +
  labs(x = "AFS", y = "Nurses") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q01-scatterplot-model.png")
