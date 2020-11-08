library(data.table)
library(ggplot2)

senic <- fread(input = 'data/SENIC.txt')

# question 1 --------------------------------------------------------------

# a- scatterplot of Y = nurses vs X = AFS
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(cex = 4) +
  geom_smooth(method = 'loess', cex = 2, aes(color = 'Loess')) +
  scale_color_manual(name = '', values = 'red') +
  theme_bw(base_size = 40) +
  # theme(legend.position = 'bottom') +
  theme(legend.position = c(0.175, 0.975), legend.background = element_blank()) +
  labs(x = 'AFS', y = 'Nurses') 
ggsave(filename = 'hwk/hwk04/img/q01a.png')
