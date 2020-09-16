library(data.table)
library(ggplot2)

# question 1 --------------------------------------------------------------

# reading in the data and creating lowess smoother
cm <- fread(input = 'data/copier_maintenance.txt')
cm <- cm[order(minutes), .(copiers, minutes)
         ][, lowess := lowess(x = cm$copiers, y = cm$minutes)[[2]]]

# plotting the data with the lowess smoother
ggplot(data = cm) +
  geom_point(mapping = aes(x = copiers, y = minutes), cex = 4, pch = 1, stroke = 1.5) +
  geom_line(mapping = aes(x = copiers, y = lowess), cex = 4, color = 'red') +
  labs(x = 'Copiers', y = 'Minutes') +
  theme_minimal(base_size = 40)
ggsave(filename = 'img/q01_lowess.png')

# fitting the linear regression model
cm_linear <- lm(minutes ~ copiers, data = cm)

# adding fitted values to the cm data.table
cm[, fitted := cm_linear$fitted.values]

# plotting the data with the linear regression line
ggplot(data = cm) +
  geom_point(mapping = aes(x = copiers, y = minutes), cex = 4, pch = 1, stroke = 1.5) +
  geom_line(mapping = aes(x = copiers, y = fitted), cex = 4, color = 'orange') +
  labs(x = 'Copiers', y = 'Minutes') +
  theme_minimal(base_size = 40)
ggsave(filename = 'img/q01_linear.png')

# confirming that the residuals sum to 0
sum(cm_linear$residuals)
