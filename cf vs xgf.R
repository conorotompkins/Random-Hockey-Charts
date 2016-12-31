install.packages("tidyverse")
library(tidyverse)
theme_set(theme_nhh())

corsica_cf <- read_csv("corsica_cf.csv")
corsica_cf <- corsica_cf[, c(2, 11)]
corsica_cf <- corsica_cf %>%
  mutate(cf_percent = `CF%`,
         team = Team) %>%
  select(team, cf_percent)

dtm_xGF <- c(54.8, 54.7, 54.2, 53.4, 53.1, 52.9, 52.7, 52.4, 52.1, 51.9, 51.6, 51.5, 51.2, 51, 50.4, 50.2, 50, 49.1, 48.7, 48.6, 48.6, 48.2, 47.8, 47.1, 46.8, 46.8, 46.6, 46.3, 45.4, 35.98)
team <- c("BOS", "PIT", "TOR", "NSH", "STL", "CBJ", "L.A", "S.J", "MIN", "CAR", "EDM", "BUF", "WSH", "NYR", "MTL", "OTT", "ANA", "CHI", "PHI", "WPG", "T.B", "FLA", "DAL", "DET", "N.J", "CGY", "NYI", "VAN", "COL", "ARI")

df <- data_frame(team, dtm_xGF) %>%
  left_join(corsica_cf)

ggplot(df, aes(cf_percent, dtm_xGF, label = team)) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_label(size = 6) +
  labs(x = "Shots For %",
       y = "Expected Goals For %",
       title = "xGF% vs Shots For %",
       subtitle = "xGF from @DTMAboutHeart, CF from corsica.hockey",
       caption = "@Null_HHockey") +
  theme(plot.caption = element_text(hjust = 1))
ggsave("cf vs xgf.png")


df %>%
  filter(team != "ARI") %>%
  ggplot(aes(cf_percent, dtm_xGF, label = team)) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 50) +
  geom_label(size = 6) +
  labs(x = "Shots For %",
       y = "Expected Goals For %",
       title = "xGF% vs Shots For %",
       subtitle = "xGF from @DTMAboutHeart, CF from corsica.hockey",
       caption = "@Null_HHockey") +
  theme(plot.caption = element_text(hjust = 1))
ggsave("cf vs xgf without ari.png")
  