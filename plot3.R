
# Plot3 is another histogram of average number of steps per day,
# this time including imputed values in place of missing values.

source("script_activity.R")

library(ggplot2)

theme <- theme_bw() +
        theme(plot.title = element_text(size = 24, hjust = .5,
                                        face = "bold", margin = margin(0,0,25,0)),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 10),
              strip.text = element_text(size = 12))

plot3 <- ggplot(total.steps2, aes(x = total.steps))
plot3 <- plot3 + geom_histogram(binwidth = 2000, boundary = 0,
                                col = "black", fill = "tomato") +
        labs(title = "Histogram of total number of steps per day",
             x = "Number of steps",
             y = "Number of days") +
        ylim(0,25) +
        theme
plot3