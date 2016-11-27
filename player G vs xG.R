library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

data <- read_csv("Corsica_Skater.Stats_11h47.csv") %>%
        filter(Team == "PIT") %>%
        arrange(Date) %>%
        mutate(iG60 = (G * TOI) / 60,
               game_number = dense_rank(Date)) %>%
        select(Player, Date, game_number, ixG60, iG60) %>%
        gather(., metric, measure, -Player, -Date, -game_number)

data$metric[data$metric == "iG60"] <- "Actual Goals per 60"
data$metric[data$metric == "ixG60"] <- "Expected Goals per 60"

ggplot(data, aes(game_number, measure, color = metric, fill = metric)) +
        geom_point(alpha = .1) +
        geom_smooth(span = .3) +
        facet_wrap(~metric) +
        coord_cartesian(ylim = c(0, 2)) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Metric",
             title = "Sidney Crosby 2007-2016",
             subtitle = "5v5",
             caption = "@Null_HHockey, data from http://www.corsica.hockey") +
        guide_legend(direction = "vertical") +
        theme(legend.position = "bottom")



