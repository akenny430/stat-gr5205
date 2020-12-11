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



# parts a and b -----------------------------------------------------------



# reading in data
jobs <- fread(
  "data/job-proficiency.txt"
) 

# fitting null and full models
# since we are using AIC as a stopping criteria, we need to use base::step()
null_model <- lm(Y ~ 1, data = jobs)
full_model <- lm(Y ~ ., data = jobs)

# running backward stepwise regression
bwd_model <- step(object = full_model, scope = formula(null_model), direction = "backward")
summary(bwd_model)

# running forward stepwise regression
fwd_model <- step(object = null_model, scope = formula(full_model), direction = "forward")
summary(fwd_model)




# part c ------------------------------------------------------------------



# using BIC as stopping criteria
# need k = long(n) instead of k = 2
# running backward stepwise regression
bwd_model <- step(object = full_model, scope = formula(null_model), direction = "backward", k = log(nrow(jobs)))
summary(bwd_model)

# running forward stepwise regression
fwd_model <- step(object = null_model, scope = formula(full_model), direction = "forward", k = log(nrow(jobs)))
summary(fwd_model)
