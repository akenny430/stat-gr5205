library(data.table)
library(ggplot2)
library(GGally)

senic <- fread(input = 'data/SENIC.txt')

# question 1 --------------------------------------------------------------

# a- scatterplot of Y = nurses vs X = AFS using loess and quadratic
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(cex = 4) +
  geom_smooth(method = 'loess', cex = 2, aes(color = 'Loess'), se = F) +
  geom_smooth(method = 'lm', formula = y ~ poly(x, degree = 2, raw = T),
              cex = 2, aes(color = 'Quadratic')) +
  scale_color_manual(name = '', values = c('#ff9900', '#66cc00')) +
  theme_bw(base_size = 40) +
  # theme(legend.position = 'bottom') +
  theme(legend.position = c(0.175, 0.96), legend.background = element_blank()) +
  labs(x = 'AFS', y = 'Nurses') 
ggsave(filename = 'hwk/hwk04/img/q01a-both.png')

# also doing one for just the loess smoother
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(cex = 4) +
  geom_smooth(method = 'loess', cex = 2, aes(color = 'Loess'), se = F) +
  scale_color_manual(name = '', values = '#ff9900') +
  theme_bw(base_size = 40) +
  # theme(legend.position = 'bottom') +
  theme(legend.position = c(0.175, 0.96), legend.background = element_blank()) +
  labs(x = 'AFS', y = 'Nurses') 
ggsave(filename = 'hwk/hwk04/img/q01a-single.png')

# b- fit quadratic model
quad_fit <- lm(Nurses ~ poly(AFS, degree = 2, raw = T), data = senic)

# c- test to see if quadratic term can be dropped
summary(quad_fit)

# d- confidence intervals when X = 30 and X = 60
predict(quad_fit, newdata = data.frame(AFS = c(30, 60)), interval = 'prediction')
0.95^2

# e- diagnostics plots
senic_alt <- senic[, .(Nurses, AFS)]
senic_alt[, c('fitted', 'residuals') := .(quad_fit$fitted.values, quad_fit$residuals)]
senic_alt <- melt(senic_alt, id.vars = 'residuals', measure.vars = c('fitted', 'AFS'))

ggplot(data = senic_alt, aes(x = value, y = residuals)) +
  geom_point(cex = 4) +
  facet_grid(. ~ variable, scales = 'free') +
  geom_hline(yintercept = 0, color = 'red', lwd = 2) +
  theme_bw(base_size = 40) +
  labs(x = 'Variable', y = 'Residuals')
ggsave(filename = 'hwk/hwk04/img/q01-residuals.png', width = 1.6*11.2, height = 8.71)

ggplot(data = senic_alt, aes(sample = residuals)) +
  geom_qq(cex = 4) +
  geom_qq_line(color = 'red', lwd = 2) +
  theme_bw(base_size = 40) +
  labs(x = 'Theoretical', y = 'Sample')
ggsave(filename = 'hwk/hwk04/img/q01-qqplot.png')




# question 2 --------------------------------------------------------------

# a- re-coding data
senic[MS == 2, MS := 0][, MS := as.factor(MS)]

# b- scatterplot matrix, Y = risk, X = Stay, Age, Xray, shape = MS
GGally::ggpairs(senic, columns = c('Risk', 'Stay', 'Age', 'Xray'), aes(pch = MS, color = MS)) + 
  # scale_color_manual(name = '', values = c('#ff9900', '#66ff33')) +
  theme_bw(base_size = 20)
ggsave(filename = 'hwk/hwk04/img/q02-correlation.png')

# c- fitting the two models
lin_fit <- lm(Risk ~ Stay + Age + Xray + MS, data = senic)
int_fit <- lm(Risk ~ Stay + Age + Xray + MS + Stay*MS + Age*MS + Xray*MS, data = senic)
anova(int_fit, lin_fit)

# d- confidence interval
confint(lin_fit, level = 0.95)





# question 3 --------------------------------------------------------------

senic[, Reg := as.factor(Reg)]

# a- scatterplot matrix, Y = Stay, X = Age, Cult, Cen, AFS, consider Region
GGally::ggpairs(senic, columns = c('Stay', 'Age', 'Cult', 'Cen', 'AFS'), aes(pch = Reg, color = Reg)) + 
  theme_bw(base_size = 20)
ggsave(filename = 'hwk/hwk04/img/q03-correlation.png')

# b- fitting linear model
lin_fit <- lm(Stay ~ Age + Cult + Cen + AFS + Reg, data = senic)

# c- confidence intervals
confint(lin_fit, level = 0.99)

# d- testing whether region has an effect
red_fit <- lm(Stay ~ Age + Cult + Cen + AFS, data = senic)
anova(lin_fit, red_fit)
