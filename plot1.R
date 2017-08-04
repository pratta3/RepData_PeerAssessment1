
# Plot1 is a histogram of the average number of steps per day
# where missing values are omitted.

source("script_activity.R")

library(ggplot2)

theme <- theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = .5,
                                        face = "bold", margin = margin(0,0,25,0)),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12))

plot1 <- ggplot(total.steps, aes(x = total.steps))
plot1 <- plot1 + geom_histogram(binwidth = 2000, color = "black",
                                fill = "royalblue", boundary = 0) +
        labs(title = "Histogram of total number of steps per day",
             x = "Number of steps",
             y = "Number of days") +
        theme +
        ylim(0,25)
plot1