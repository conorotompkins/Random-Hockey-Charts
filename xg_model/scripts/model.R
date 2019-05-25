library(tidyverse)
library(tidymodels)
library(janitor)

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
  ggplot(aes(coords_x, coords_y)) +
  geom_point() +
  facet_wrap(~shot_type)

df %>% 
  ggplot(aes(coords_x, coords_y)) +
  #geom_point()
  stat_density_2d(aes(fill = stat(level)), geom = "polygon") +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(coords_x)) +
  geom_histogram()

summary(df$coords_y)

df %>% 
  ggplot(aes(coords_y)) +
  geom_histogram()

min(df$coords_x)
max(df$coords_x)
min(df$coords_y)
max(df$coords_y)


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


df_model <- df %>% 
  #mutate(goal = as.numeric(goal)) %>% 
  select(goal, gametime, event_type, shot_type, 
         coords_x, coords_y, home_skaters, away_skaters, 
         game_score_state, game_strength_state, event_distance, event_angle) %>% 
  mutate_if(is.character, factor) %>% 
  select(goal, event_distance, event_angle, shot_type)

df_model %>% 
  ggplot(aes(goal)) +
  geom_histogram(stat = "count")

df_model %>% 
  mutate(goal = as.numeric(goal)) %>% 
  ggplot(aes(event_distance, goal, color = shot_type, fill = shot_type)) +
  #geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(limits = c(0, 1))

df_model %>% 
  mutate(goal = as.numeric(goal)) %>% 
  ggplot(aes(event_angle, goal, color = shot_type, fill = shot_type)) +
  #geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_y_continuous(limits = c(0, 1))

df_model %>% 
  ggplot(aes(event_distance, event_angle)) +
  geom_point()

df_aug %>% 
  select(goal, event_distance, event_angle, .fitted, .predicted, .resid) %>% 
  View()

model <- glm(goal ~ ., data = df_model, family = "binomial")

model %>% 
  tidy()

model %>% 
  glance()

df_aug <- model %>% 
  augment(type.predict = "response") %>% 
  mutate(.predicted = case_when(.fitted >= .25 ~ TRUE,
                                .fitted < .25 ~ FALSE))

df_aug %>% 
  ggplot(aes(.resid)) +
  geom_density()

df_aug %>% 
  ggplot(aes(.fitted)) +
  geom_density()

metrics(df_aug, goal, .predicted)

df_aug %>% 
  mutate(goal = as.factor(goal),
         .predicted = as.factor(.predicted)) %>% 
  recall(goal, .predicted)
recall(model)
?recall
