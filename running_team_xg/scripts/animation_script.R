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

#animated scatter plot
p3 <- df_smoothed %>% 
  filter(season == "2017_2018") %>% 
  ggplot(aes(x = xgf_loess, y = xga_loess, fill = team, label = team, frame = game_number)) +
  geom_abline() +
  geom_vline(xintercept = mean(df_smoothed$xgf60)) +
  geom_hline(yintercept = mean(df_smoothed$xga_loess)) +
  geom_label(aes(cumulative = FALSE, group = team), show.legend = FALSE) +
  geom_path(aes(cumulative = TRUE, group = team, color = team)) +
  coord_equal(xlim = c(1.75, 3), ylim = c(1.75, 3)) +
  scale_y_reverse() +
  scale_color_discrete("Team") +
  labs(x = "Expected Goals For",
       y = "Expected Goals Against",
       title = "2017-2018 All Situations",
       caption = "@Null_HHockey, data from corsica.hockey")
p3
#gganimate(p3, interval = .1)
gganimate(p3, interval = .25, title_frame = FALSE,  "xg_scatter_plot_animated.gif", ani.width=1000, ani.height=1000)
