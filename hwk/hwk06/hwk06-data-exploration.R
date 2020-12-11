library(data.table)


# reading in data ---------------------------------------------------------


flights <- fread(
  input = "https://raw.githubusercontent.com/ismayc/pnwflights14/master/data/flights.csv"
)[
  , na.omit(flights)
]


# data exploration --------------------------------------------------------


names(flights)

# "year"       : year that the flight occurred (all 2014)
# "month"      : month that the flight occurred 
# "day"        : day that the flight occurred 
# "dep_time"   : time of departure in minutes, e.g. 2111 is 9:11 PM
# "dep_delay"  : delay of flight departure in minutes, negative means flight leaves early
# "arr_time"   : time of arrival in minutes, e.g. 2111 is 9:11 PM
# "arr_delay"  : delay of flight arrivals in minutes, negative means flight arrives early
# "carrier"    : guessing this is airline of the plane giving the flight in IATA
# "tailnum"    : specific tail number of the flight
# "flight"     : specific flight number of given flight
# "origin"     : location of departure, only has PDX and SEA
# "dest"       : location of arrival
# "air_time"   : duration of flight in minutes
# "distance"   : distance of flight in miles
# "hour"       : hour of departure time (military time)
# "minute"     : minute of departure time

# notes / things to try out:
# - if specific airlines have more delays (regional vs mainline vs whatever else)
# - depending on location of dest, makes flights more delayed, maybe break down by region of country
# - actually have distance of flight given to us
# - maybe time of year has impact, could check to see if volume of flights changes by month

# side tasks to check
# - find out number of flights per month -> NOT RELEVANT
# - break carriers down into sub-classes (e.g. mainline, regional)
# - will specific plane have longer delay time?
# - total number of "delays" = # of flights with positive dep_delay
# - does one airport end up having more delays?
# - flights with positive dep_delay but negative arr_delay and vice-verse

# potential model variables:
# - we are predicting arr_delay
# - distance
# - month
# - carrier

# things to drop and why:
# - year: since all flights are from 2014, obviously won't have any impact
# - tailnum: will specific plane have longer delays?



# new variables,  binary dep_array and arr_delay --------------------------


flights[
  , dep_delay_int := ifelse(dep_delay > 0, 1, 0)
][]


# getting number of flights per month -------------------------------------


flights[
  , .(month)
][
  , .N, by = .(month)
]






