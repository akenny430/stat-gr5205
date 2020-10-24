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
