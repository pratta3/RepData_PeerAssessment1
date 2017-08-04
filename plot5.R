
# Plot5 is a time-series plot showing the average number of steps
# at different times throughout the day on weekdays and on weekends.

source("script_activity.R")

wday.patterns <- activity2 %>% 
        group_by(day.type, time) %>% 
        summarize(mean = mean(steps)) %>% 
        arrange(day.type, time)

theme <- theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = .5,
                                        face = "bold", margin = margin(0,0,25,0)),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12))

library(ggplot2)
labels <- c(weekday = "Weekdays", weekend = "Weekends")
plot5 <- ggplot(wday.patterns, aes(time, mean))
plot5 <- wday.plot + geom_line() +
        facet_wrap(~day.type, nrow = 2,
                   labeller = labeller(day.type = labels),
                   strip.position = "right") +
        scale_x_datetime(date_labels = "%H:%M") +
        labs(title = "Average number of steps taken throughout the day \non weekdays and weekends",
             x = "Time of day",
             y = "Number of steps") +
        theme
plot5