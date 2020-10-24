library(data.table)
library(ggplot2)
library(car)


# question 1 --------------------------------------------------------------

cdata <- fread(input = 'data/copier-maintenance.txt')

# fitting model
clm <- lm(minutes ~ copiers, data = cdata)

# (a) finding confidence interval when X = 6
predict(clm, newdata = data.table(copiers = 6), interval = 'confidence')
#        fit     lwr      upr
# 1 89.63133 86.8152 92.44746

# (b) finding prediction interval when X = 6
predict(clm, newdata = data.table(copiers = 6), interval = 'prediction')
# ```````fit      lwr      upr
# 1 89.63133 71.43628 107.8264

# (d) and (e) setting up anova table and getting p value
anova(clm)
# Analysis of Variance Table
# 
# Response: minutes
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# copiers    1  76960   76960  968.66 < 2.2e-16 ***
# Residuals 43   3416      79                      
# ---

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
