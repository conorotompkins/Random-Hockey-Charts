library(dplyr)
library(ggplot2)
library(readr)

df <- read_csv("fehr.csv") %>%
        arrange(Date) %>%
        mutate(rel_cf = `Rel.CF%`,
               game_number = dense_rank(Date))

test <- df %>%
        filter(Team == "WPG")

unique(df$Team)

ggplot(df, aes(game_number, rel_cf)) +
        geom_hline(yintercept = 0) +
        geom_point(alpha = I(.1)) +
        geom_smooth() +
        geom_vline(xintercept = 467) +
        annotate("text", x = 400, y = 25, label = "Move to PIT") +
        labs(x = "Game Number",
             y = "Relative Corsi For %",
             title = "Eric Fehr",
             caption = "@Null_HHockey, data from http://www.corsica.hockey/") +
        theme_bw()
ggsave("Fehr.png")
?annotate
