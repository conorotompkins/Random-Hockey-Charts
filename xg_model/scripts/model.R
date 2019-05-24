library(tidyverse)
library(tidymodels)
library(janitor)

theme_set(theme_bw())

df <- read_csv("xg_model/data/Shot Prediction Data for Comparison.csv") %>% 
  clean_names()


df %>% 
  ggplot(aes(coords_x, coords_y)) +
  #geom_point()
  stat_density_2d(aes(fill = goal, geom = "polygon")) +
  scale_fill_viridis_c()

df %>% 
  ggplot(aes(coords_x)) +
  geom_histogram()

summary(df$coords_y)

df %>% 
  ggplot(aes(coords_y)) +
  geom_histogram()

df_shooting_tile <- df %>% 
  select(coords_x, coords_y, goal) %>% 
  complete(coords_x, coords_y) %>% 
  replace_na(list(goal = FALSE)) %>% 
  group_by(coords_x, coords_y) %>% 
  summarize(shoot_percent = mean(goal == TRUE),
            n = n())

df_shooting_tile %>% 
  ungroup() %>% 
  summarize(rows = sum(n))

df_shooting_tile %>% 
  ggplot(aes(n, shoot_percent)) +
  geom_point()

df_shooting_tile %>% 
  ggplot(aes(coords_x, coords_y, fill = shoot_percent)) +
    geom_density_2d() +
    scale_fill_viridis_c()

df_model <- df %>% 
  #mutate(goal = as.numeric(goal)) %>% 
  select(goal, gametime, event_type, shot_type, 
         coords_x, coords_y, home_skaters, away_skaters, 
         game_score_state, game_strength_state, event_distance, event_angle) %>% 
  mutate_if(is.character, factor) %>% 
  select(goal, event_distance, event_angle)

df_model %>% 
  ggplot(aes(goal)) +
  geom_histogram()

df_model %>% 
  ggplot(aes(event_distance, goal)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))


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
