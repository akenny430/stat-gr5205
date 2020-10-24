library(data.table)
library(ggplot2)
library(GGally)


# question 1 --------------------------------------------------------------

cdata <- fread(input = 'data/copier-maintenance.txt')

# fitting model
clm <- lm(minutes ~ copiers, data = cdata)
summary(clm)

# (a) finding confidence interval when X = 6
predict(clm, newdata = data.table(copiers = 6), interval = 'confidence')

# (b) finding prediction interval when X = 6
predict(clm, newdata = data.table(copiers = 6), interval = 'prediction')

# (d) and (e) setting up anova table and getting p value
anova(clm)

# making plot of model
ggplot(data = cdata, aes(x = copiers, y = minutes)) +
  geom_point(aes(color = "Observations"), cex = 4, pch = 1, stroke = 1.5) +
  # geom_smooth(method = 'lm', color = 'red', cex = 2) +
  geom_smooth(aes(color = 'Minutes'), method = 'lm', cex = 2) +
  labs(x = 'Copiers', y = 'Minutes', color = '') +
  scale_shape_manual(values = c(3, 7)) +
  scale_color_manual(values = c('#f8766d', 'black')) +
  theme_bw(base_size = 40) +
  theme(legend.position = 'bottom')
ggsave(filename = 'hwk/hwk03/img/copier-linear.png')



# question 2 --------------------------------------------------------------

# reading in data 
psdata <- fread(input = 'data/patient-satisfaction.txt')

# (a) making scatterplot matrix
GGally::ggpairs(psdata[, -1]) +
  theme_bw(base_size = 15)
ggsave(filename = 'hwk/hwk03/img/ps-correlation.png')

# (b) fitting the multiple regression model
pslm <- lm(Y ~ Age + Severity + Anxiety, data = psdata)
summary(pslm)

# (c) plotting residuals against each of the predictor variables

# psdata[, .residuals := pslm$residuals]
psdata[, c('fitted', 'residuals') := .(pslm$fitted.values, pslm$residuals)]
psnew <- melt(psdata, id.vars = 'residuals', measure.vars = c('fitted', 'Age', 'Severity', 'Anxiety'))

ggplot(data = psnew, aes(x = value, y = residuals)) +
  geom_point(cex = 4) +
  facet_grid(. ~ variable, scales = 'free') +
  geom_hline(yintercept = 0, color = 'red', lwd = 2) +
  theme_bw(base_size = 40) +
  labs(x = 'Variable', y = 'Residuals')
ggsave(filename = 'hwk/hwk03/img/ps-residuals.png', width = 2*11.2, height = 8.71)


# (d) making a qq-plot

ggplot(data = psnew, aes(sample = residuals)) +
  geom_qq(cex = 4) +
  geom_qq_line(color = 'red', lwd = 2) +
  theme_bw(base_size = 40) +
  labs(x = 'Theoretical', y = 'Sample')
ggsave(filename = 'hwk/hwk03/img/ps-qqnorm.png')






# question 3 --------------------------------------------------------------

# (a) getting the F value
summary(pslm)

# (b) getting the confidence intervals for each coefficient
confint(pslm, level = 0.9)

# (d) and (e) getting confidence and prediction intervals
predict(pslm, newdata = data.frame(Age = 35, Severity = 45, Anxiety = 2.2), interval = 'confidence')
predict(pslm, newdata = data.frame(Age = 35, Severity = 45, Anxiety = 2.2), interval = 'prediction')
