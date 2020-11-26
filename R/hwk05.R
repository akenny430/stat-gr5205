myred    <- "#ff6666"
myorange <- "#ffcc00"
myyellow <- "#ffff00"
mygreen  <- "#99ff33"
myblue   <- "#99ccff"
mypink   <- "#ff99ff"
library(data.table)
library(ggplot2)

# question 1 --------------------------------------------------------------

senic <- fread(input = "data/SENIC.txt")

# histogram and histograms for nurses
ggplot(senic, aes(x = log(Nurses), y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myblue) +
  labs(x = "Nurses", y = "Density") +
  theme_bw(base_size = 20)

ggplot(senic, aes(y = Nurses)) +
  geom_boxplot(fill = myblue) +
  labs(x = "", y = "Nurses") +
  theme_bw(base_size = 20)

# histogram and scatterplot for nurses
ggplot(senic, aes(x = AFS, y = ..density..)) +
  geom_histogram(bins = 20, boundary = 0, fill = myorange) +
  labs(x = "AFS", y = "Density") +
  theme_bw(base_size = 20)

ggplot(senic, aes(y = AFS)) +
  geom_boxplot(fill = myorange) +
  labs(x = "", y = "AFS") +
  theme_bw(base_size = 20)

# making scatterplot
ggplot(senic, aes(x = AFS, y = Nurses)) +
  geom_point(cex = 2) +
  labs(x = "AFS", y = "Nurses") +
  theme_bw(base_size = 20)

ggplot(senic, aes(x = AFS, y = log(Nurses))) +
  geom_point(cex = 2) +
  labs(x = "AFS", y = "log(Nurses)") +
  theme_bw(base_size = 20)

# general observations: 
# - Nurses could be gamma distributed
# - AFS looks normally distributed
# - looking at scatterplot, maybe log transform?
