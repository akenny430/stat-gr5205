library(data.table)
library(ggplot2)

# question 1 --------------------------------------------------------------

# reading in the data
cm <- fread(input = 'data/copier_maintenance.txt')

# plotting the data
ggplot(data = cm) +
  geom_point(mapping = aes(x = minutes, y = copiers), cex = 2) +
  theme_gray(base_size = 30)
ggsave(filename = 'img/q01_lowess.png')
