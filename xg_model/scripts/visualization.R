library(tidyverse)
library(janitor)
library(gganimate)

theme_set(theme_bw())

df <- read_csv("xg_model/data/Shot Prediction Data for Comparison.csv") %>% 
  clean_names()

df %>% 
  #filter(str_detect(event_player_1, "CROSBY") | str_detect(event_player_1, "OVECHKIN")) %>% 
  mutate(coords_x = case_when(coords_x < 0 ~ coords_x * -1,
                              coords_x >= 0 ~ coords_x)) %>% 
  ggplot(aes(coords_x, coords_y)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon", alpha = .5) +
  #facet_wrap(~event_player_1, ncol = 1) +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(coords_x)) +
  geom_density()

df %>% 
  ggplot(aes(coords_y)) +
  geom_density()

df %>% 
  ggplot(aes(coords_x, coords_y)) +
  #geom_point()
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(coords_x, coords_y)) +
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c() +
  facet_wrap(~shot_type) 

df %>% 
  ggplot(aes(coords_x, coords_y)) +
  geom_point() +
  #stat_density_2d(aes(alpha = stat(level), fill = shot_type), geom = "polygon") +
  scale_fill_viridis_d() +
  transition_states(shot_type) +
  enter_fade() + 
  exit_shrink() +
  ease_aes("linear")

df %>% 
  ggplot(aes(coords_x)) +
  geom_histogram()

df_shooting_tile <- df %>% 
  select(coords_x, coords_y, goal) %>% 
  group_by(coords_x, coords_y) %>% 
  summarize(shoot_percent = mean(goal == TRUE),
            n = n()) %>% 
  complete(coords_x = c(-98:98), coords_y = c(-50:50)) %>% 
  replace_na(list(shoot_percent = 0, n = 0))

df_shooting_tile %>% 
  ungroup() %>% 
  summarize(rows = sum(n))

df_shooting_tile %>% 
  ggplot(aes(n, shoot_percent)) +
  geom_point()

df_shooting_tile %>% 
  ggplot(aes(coords_x, coords_y, fill = shoot_percent, alpha = n)) +
  geom_tile() +
  #stat_density_2d(aes(fill = shoot_percent), geom = "polygon") +
  scale_fill_viridis_c() +
  theme_void()

df %>% 
  ggplot(aes(coords_x, coords_y, fill = event_angle)) +
  geom_tile() +
  scale_fill_viridis_c()