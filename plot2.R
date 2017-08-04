
# Plot2 is a time-series plot showing the average number of steps at different
# time intervals throughout the day.

source("script_activity.R")

library(ggplot2)

theme <- theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = .5,
                                        face = "bold", margin = margin(0,0,25,0)),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12))

plot2 <- ggplot(pattern.steps, aes(time, mean))
plot2 <- plot2 + geom_line() +
        scale_x_datetime(date_labels = "%H:%M") +
        labs(title = "Average number of steps taken throughout the day",
             x = "Time of day",
             y = "Number of steps") +
        theme
plot2