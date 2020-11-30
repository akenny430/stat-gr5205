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


# reading in data for nurses and AFS
senic <- fread(
  input = "data/SENIC.txt"
)[, .(Age, Risk, Cult, Xray, Beds, Cen, Nurses, AFS, Stay)]

