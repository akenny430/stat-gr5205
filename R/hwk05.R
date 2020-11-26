myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mypink   <- "#ff99ff"
library(data.table)
library(ggplot2)

# question 1 --------------------------------------------------------------

senic <- fread(input = "data/SENIC.txt")

# histogram and histograms for nurses
ggplot(senic, aes(x = log(Nurses), y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myblue) +
  labs(x = "Nurses", y = "Density") +
  theme_bw(base_size = 20)

ggplot(senic, aes(y = Nurses)) +
  geom_boxplot(fill = myblue) +
  labs(x = "", y = "Nurses") +
  theme_bw(base_size = 20)

# histogram and scatterplot for nurses
ggplot(senic, aes(x = AFS, y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myorange) +
  labs(x = "AFS", y = "Density") +
  theme_bw(base_size = 20)

ggplot(senic, aes(y = AFS)) +
  geom_boxplot(fill = myorange) +
  labs(x = "", y = "AFS") +
  theme_bw(base_size = 20)

# making scatterplot
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(cex = 2) +
  labs(x = "AFS", y = "Nurses") +
  theme_bw(base_size = 20)

ggplot(senic, aes(x = AFS, y = log(Nurses))) +
  geom_point(cex = 2) +
  labs(x = "AFS", y = "log(Nurses)") +
  theme_bw(base_size = 20)

# general observations: 
# - Nurses could be gamma distributed
# - AFS looks normally distributed
# - looking at scatterplot, maybe log transform?
# - log transform looks good, it makes histogram more normal and scatterplot looks linear

# function for box-cox transform
box_cox_trans <- function(y, lambda = 0) {
  if (lambda == 0) {
    log(y)
  } else {
    (y^lambda - 1) / lambda
  }
}

# function for log likelihood value depending on lambda
max_mle <- function(lambda, y, X) {
  n        <- length(y)
  D        <- cbind(rep(1, length(y)), X)
  A        <- diag(n) - (D %*% solve(t(D) %*% D) %*% t(D))
  y_lamb   <- box_cox_trans(y, lambda)
  sig_lamb <- t(y_lamb) %*% A %*% y_lamb / n
  const    <- -n * log(2 * pi * exp(1) / n) / 2
  max_lamb <- const - n * log(sig_lamb) / 2 + (lambda - 1) * sum(log(y))
  return(max_lamb)
}

# finding the best lambda
try_lambda <- seq(-1, 1, 0.005)
max_mle_vals <- rep(NA, length(try_lambda))
for (i in 1:length(try_lambda)) {
  max_mle_vals[i] <- max_mle(try_lambda[i], senic$Nurses, senic$Age)
}
lambda_mles <- data.table(lambda = try_lambda, loglikelihood = max_mle_vals)
best_lamb <- lambda_mles[which.max(lambda_mles$loglikelihood)]

# making the plot
ggplot(lambda_mles, aes(x = lambda, y = loglikelihood)) +
  geom_point(cex = 1) +
  geom_point(data = best_lamb, mapping = aes(x = lambda, y = loglikelihood), color = myred, cex = 3) +
  labs(x = expression(lambda), y = "Log-Likelihood") +
  theme_bw(base_size = 20)

# tried this but didn't work, will probably have to write function better
# ggplot(data.table(x = seq(-2, 2, 0.01)), aes(x)) +
#   geom_function(fun = max_mle, args = list(y = senic$Nurses, X = senic$AFS))
