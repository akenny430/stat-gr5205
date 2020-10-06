library(data.table)

# question 4 --------------------------------------------------------------

# reading in data
cm <- fread(input = 'data/copier-maintenance.txt')

# part (a): fitting the linear model 
cm_lin <- lm(minutes ~ copiers, data = cm)
summary(cm_lin)

# part (b): finding confidence interval
confint(cm_lin)[2,]

# part (c): running our own hypothesis test
n <- nrow(cm)
b1_null <- 14.0
b1_est <- as.numeric(cm_lin$coefficients[2])
b1_std <- as.numeric(summary(cm_lin)$coefficients[2, 2])
t_stat <- (b1_est - b1_null) / b1_std
(p_val <- 1 - pt(q = t_stat, df = n - 2))
p_val <= 0.05