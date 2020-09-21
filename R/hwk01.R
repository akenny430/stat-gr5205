library(data.table)
library(ggplot2)

# question 1 --------------------------------------------------------------

# reading in the data and creating lowess smoother
cm <- fread(input = 'data/copier_maintenance.txt')

# part (a): plotting the data with the lowess smoother
ggplot(data = cm, mapping = aes(x = copiers, y = minutes)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'loess', color = 'red', cex = 2) +
  labs(x = 'Copiers', y = 'Minutes') +
  theme_minimal(base_size = 40)
ggsave(filename = 'img/q01_lowess.png')

# part (b): plotting the data with the linear regression line
ggplot(data = cm, mapping = aes(x = copiers, y = minutes)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'lm', color = 'orange', cex = 2) +
  labs(x = 'Copiers', y = 'Minutes') +
  theme_minimal(base_size = 40)
ggsave(filename = 'img/q01_linear.png')

# fitting the linear regression model
cm_linear <- lm(minutes ~ copiers, data = cm)

# getting model coefficients
cm_linear$coefficients

# part (e)/(f): getting a point estimate/prediction at X = 5
predict(cm_linear, newdata = data.table(copiers = 5))

# part (g): confirming that the residuals sum to 0
sum(cm_linear$residuals)

# part (h): getting point estimate for sigma^2 and sigma
var(x = cm_linear$residuals) # wrong way... divides by n - 1
sum(cm_linear$residuals^2)/(nrow(cm) - 2) # right way
sqrt(sum(cm_linear$residuals^2)/(nrow(cm) - 2))


# question 2 --------------------------------------------------------------

# reading in data
sen <- fread(input = 'data/SENIC.txt')

# part (a): loess smoothers
ggplot(data = sen, mapping = aes(x = Risk, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'loess', color = 'red', cex = 2) +
  labs(x = 'Patient Risk', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_loess1.png')

ggplot(data = sen, mapping = aes(x = AFS, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'loess', color = 'red', cex = 2) +
  labs(x = 'Available Services', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_loess2.png')

ggplot(data = sen, mapping = aes(x = Xray, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'loess', color = 'red', cex = 2) +
  labs(x = 'X-ray Ratio', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_loess3.png')

# part (b): linear model plots
ggplot(data = sen, mapping = aes(x = Risk, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'lm', color = 'orange', cex = 2) +
  labs(x = 'Patient Risk', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_lm1.png')

ggplot(data = sen, mapping = aes(x = AFS, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'lm', color = 'orange', cex = 2) +
  labs(x = 'Available Services', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_lm2.png')

ggplot(data = sen, mapping = aes(x = Xray, y = Stay)) +
  geom_point(cex = 4, pch = 1, stroke = 1.5) +
  geom_smooth(method = 'lm', color = 'orange', cex = 2) +
  labs(x = 'X-ray Ratio', y = 'Average Stay') +
  theme_minimal(base_size = 40)
ggsave(filename = 'hwk/q02_lm3.png')

# fitting the linear models for each to get information
sen_lm1 <- lm(Stay ~ Risk, data = sen)
summary(sen_lm1)
mean(sen_lm1$residuals^2)

sen_lm2 <- lm(Stay ~ AFS, data = sen)
summary(sen_lm2)
mean(sen_lm2$residuals^2)

sen_lm3 <- lm(Stay ~ Xray, data = sen)
summary(sen_lm3)
mean(sen_lm3$residuals^2)
