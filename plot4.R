
# Plot4 puts plot1 and plot3 side by side for comparison

source("script_activity.R")

library(ggplot2)
library(cowplot)

plot1.3 <- ggplot(total.steps, aes(total.steps)) + 
        geom_histogram(binwidth = 2000, boundary = 0, color = "black", fill = "royalblue") +
        ylim(0, 25) +
        labs(x = "Number of steps",
             y = "Number of days")
plot3.1 <- ggplot(total.steps2, aes(total.steps)) + 
        geom_histogram(binwidth = 2000, boundary = 0, color = "black", fill = "tomato") +
        ylim(0, 25) +
        labs(x = "Number of steps",
             y = "Number of days")
plot4 <- plot_grid(plot1.3, plot3.1, align = "h", labels = c("NA's omitted", "Imputed values"),
                     hjust = -.75)
plot4