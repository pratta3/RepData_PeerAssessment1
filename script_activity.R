
# Make sure to move the "RepData_PeerAssessment1" directory into your working directory.

# Read data into R #
file <- unz(description = "./activity.zip", filename = "activity.csv")
activity <- read.csv(file = file)

library(lubridate)
library(dplyr)
library(stringr)

# Variables added to the 'activity' dataset:
#       date.time - combines date and interval into datetime
#       day.type - takes on a value of either 'weekday' or 'weekend'
#       time - (not meaningful) contains time used for plotting only

ndays <- length(unique(activity$date)) # There are 61 dates in the dataset

nintervals <- activity %>%              # Each date has the same number of observations
        count(date) %>% 
        select(n) %>% 
        first %>% 
        first


# The variable 'intervals' has a funky format, so let's fix it and
# combine it with 'date' to create a 'date.time' variable.
##  NOTE: an additional 'time' variable was also created that will help
##  make the plots prettier later.

activity <- activity %>% 
        mutate(date = ymd(date),
               minutes = rep(seq(from = 0, to = (nintervals - 1)*5, by = 5), times = ndays),
               hours = floor(minutes/60),
               minutes = minutes - (60 * hours),
               hms = str_c(hours, minutes, 0, sep = ":"),
               date.time = as.POSIXct(date + hms(hms)),
               time = as.POSIXct(hms, format = "%H:%M:%S"),
               day.type = ifelse(wday(date.time) %in% 2:6, "weekday", "weekend")) %>% 
        select(-c(minutes, hours, hms))


# Calculate total number of steps/day (NA's removed)
total.steps <- activity %>% 
        filter(is.na(steps) == FALSE) %>% # Note that some dates had ALL missing values
        group_by(date) %>% 
        summarize(total.steps = sum(steps))



# ===================

### SEE PLOT1 CODE ###

# ===================



# Calculate mean and median of total number of steps/day (NA's still removed)
mm.total.steps <- total.steps %>%
        summarize(mean = mean(total.steps),
                  median = median(total.steps))

# Calculate the mean number of steps for each time interval throughout the day.
pattern.steps <- activity %>% 
        filter(is.na(steps) == FALSE) %>%  
        group_by(time) %>% 
        summarize(mean = mean(steps)) %>%
        arrange(time)



# ===================

### SEE PLOT2 CODE ### 

# ===================



# Find which 5-minute interval contains the maximum number of steps on average.
pattern.steps[which.max(pattern.steps$mean),]


# Calculate the total number of missing values in the dataset.
sum(!complete.cases(activity))

# The missing values all occur for entire days, so it's probably fine to impute the mean
# value of each interval into these days.
missing <- activity[!complete.cases(activity),]
missing %>% count(date)

# Impute mean values for each NA in 'steps' according to the time interval.
activity2 <- activity %>% 
        left_join(pattern.steps, by = "time") %>% 
        mutate(steps = ifelse(is.na(steps) == TRUE, mean, steps)) %>% 
        select(-mean)

total.steps2 <- activity2 %>% 
        group_by(date) %>% 
        summarize(total.steps = sum(steps))



# ====================

### SEE PLOT3 CODE ###

# ====================


# ====================

### SEE PLOT4 CODE ###

# ====================



# Calculate the new mean and median of total steps/day
mm2 <- total.steps2 %>% 
        summarize(mean = mean(total.steps),
                  median = median(total.steps))

# Imputing values hasn't changed the mean or median at all
mm2
mm.total.steps



# ===================

### SEE PLOT5 CODE ###

# ===================




