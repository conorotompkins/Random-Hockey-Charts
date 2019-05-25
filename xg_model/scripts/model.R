library(tidyverse)
library(tidymodels)
library(janitor)

theme_set(theme_bw())

df <- read_csv("xg_model/data/Shot Prediction Data for Comparison.csv") %>% 
  clean_names()

df_model <- df %>% 
  #mutate(goal = as.numeric(goal)) %>% 
  select(goal, gametime, event_type, shot_type, 
         coords_x, coords_y, home_skaters, away_skaters, 
         game_score_state, game_strength_state, event_distance, event_angle) %>% 
  mutate_if(is.character, factor) %>% 
  select(goal, event_distance, event_angle, shot_type)

df_model %>% 
  ggplot(aes(goal)) +
  geom_bar(stat = "count")

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
