library(tidyverse)
library(animation)
library(gganimate)

p <- df %>%
  filter(season == "2017_2018") %>% 
  mutate(highlight_team = team == "PIT") %>% 
  ggplot(aes(x = game_number, y = xg_pm_cum, color = team, frame = game_number, group = team, size = highlight_team)) +
  geom_path(aes(cumulative = TRUE, group = team)) +
  #geom_label(data = filter(df, team == "PIT"), aes(label = team, size = NULL)) +
  scale_size_manual(values = c(.5, 2))
p
gganimate(p, interval = .1)



df_smoothed <- df %>% 
  group_by(team, season) %>% 
  mutate(xgf_loess = predict(loess(xgf60 ~ game_number)),
         xga_loess = predict(loess(xga60 ~ game_number)))


p2 <-  df_smoothed %>% 
  ggplot(aes(x = game_number, y = xgf_loess, color = team, frame = game_number)) + 
  geom_line(aes(cumulative = TRUE)) +
  facet_wrap(~season, ncol = 1) +
  labs(y = "Expected Goals For")
gganimate(p2, interval = .1, "xg_animated.gif", ani.width=1000, ani.height=800)
