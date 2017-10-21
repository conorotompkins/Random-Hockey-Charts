library(tidyverse)
library(lubridate)
library(gghighlight)

theme_set(theme_bw())

season_1617 <- read_csv("running_team_xg/data/2016_2017.csv")

season_1617

df <- season_1617 %>% 
  mutate(team = Team,
         date = ymd(Date),
         xg_pm = `xG+/-`,
         situation = "ES") %>% 
  select(situation, team, date, xg_pm) %>% 
  arrange(team, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date),
         xg_pm_cum = cumsum(xg_pm))

df %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, group = team), last(abs(xg_pm_cum)) > 20) +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0)) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.ca",
       title = "2016-2017 NHL",
       subtitle = "Even Strength")

df %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, group = team), unique(team) == "PIT") +
  #scale_x_continuous(expand = c(0,0)) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential")
