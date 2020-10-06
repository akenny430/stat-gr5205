library(data.table)

# question 4 --------------------------------------------------------------

# reading in data
cm <- fread(input = 'data/copier-maintenance.txt')

cm_lin <- lm(minutes ~ copiers, data = cm)
