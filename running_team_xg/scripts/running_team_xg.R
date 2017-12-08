library(tidyverse)
library(lubridate)
library(gghighlight)

theme_set(theme_bw())

data <- read_csv("running_team_xg/data/all_sit_season_2017_2018.csv")

df <- data %>%
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
  gghighlight_line(aes(game_number, xg_pm_cum, group = team), last(abs(xg_pm_cum)) > 8) +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.hockey",
       title = "2017-2018 NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())
ggsave("running_team_xg/images/team_xg_pm_cum.png", width = 12, height = 6)

df %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, group = team), unique(team) == "PIT") +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.hockey",
       title = "2017-2018 NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())
ggsave("running_team_xg/images/PIT_xg_pm_cum.png", width = 12, height = 6)

df %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, group = team), unique(team) == "WSH") +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.hockey",
       title = "2017-2018 NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())
ggsave("running_team_xg/images/VAN_xg_pm_cum.png", width = 12, height = 6)
