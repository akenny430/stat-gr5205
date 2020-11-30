myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mypink   <- "#ff99ff"
dark1   <- "#565657"
dark2   <- "#808081"
dark3   <- "#aaaaab"
library(data.table)
library(ggplot2)



# part a ------------------------------------------------------------------



# reading in data
jobs <- fread(
  "data/job-proficiency.txt"
) 

# making boxplots
ggplot(
  melt(jobs, measure.vars = c("X1", "X2", "X3", "X4")),
  aes(x = variable, y = value)
) +
  geom_boxplot(fill = dark3, color = dark1, cex = 1, outlier.shape = 1, outlier.size = 2, outlier.stroke = 2) +
  scale_x_discrete(labels = c("1", "2", "3", "4")) +
  labs(x = "Test", y = "Score") +
  theme_bw(base_size = 30)
ggsave("hwk/hwk05/img/q03-boxplot.png")
