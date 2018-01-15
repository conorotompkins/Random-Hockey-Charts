library(tidyverse)
library(lubridate)
library(gghighlight)

theme_set(theme_bw())

rm(list = ls())

files <- list.files(path = "running_team_xg/data", pattern = "season")
data <- lapply(paste0("running_team_xg/data/", files), read_csv)
names(data) <- list("2014_2015", "2015_2016", "2016_2017", "2017_2018")
names(data)
df <- bind_rows(data, .id = "season")

df <- df %>% 
  mutate(team = Team,
         date = ymd(Date),
         xg_pm = `xG+/-`,
         xgf60 = `xGF/60`,
         xga60 = `xGA/60`,
         situation = "All Situations") %>% 
  select(season, situation, team, date, xg_pm, xgf60, xga60) %>% 
  arrange(team, date) %>% 
  group_by(season, team) %>% 
  mutate(game_number = dense_rank(date),
         xg_pm_cum = cumsum(xg_pm)) %>% 
  ungroup() %>% 
  mutate(is_current_season = (season == "2017_2018"))

df %>% 
  filter(season == "2017_2018") %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, color = team), last(abs(xg_pm_cum)) > 15) +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  facet_wrap(~season,
             ncol = 1) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.ca",
       title = "NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())
ggsave("running_team_xg/images/2017_2018_xg_pm_cum.png", width = 12, height = 6)

df %>% 
  filter(season == "2017_2018") %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, color = team), unique(team) == "PIT") +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  facet_wrap(~season,
             ncol = 1) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.ca",
       title = "NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())
ggsave("running_team_xg/images/2017_2018_PIT_xg_pm_cum.png", width = 12, height = 6)

###metro division
df %>% 
  filter(season == "2017_2018") %>% 
  gghighlight_line(aes(game_number, xg_pm_cum, color = team), unique(team) %in% c("PIT", "WSH", "CBJ", "CAR", "NYR", "NYI", "N.J", "PHI")) +
  geom_hline(yintercept = 0,
             size = .25,
             linetype = 2) +
  scale_x_continuous(expand = c(0.01,0),
                     breaks = 1:max(df$game_number)) +
  facet_wrap(~season,
             ncol = 1) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.ca",
       title = "NHL",
       subtitle = "All Situations") +
  theme(panel.grid.minor = element_blank())

df %>% 
  filter(team %in% c("PIT", "WSH", "CBJ", "CAR", "NYR", "NYI", "N.J", "PHI")) %>% 
  ggplot(aes(game_number, xg_pm_cum, color = season, alpha = is_current_season)) +
  geom_hline(yintercept = 0, 
             linetype = 2) +
  geom_line(size = 1) +
  #geom_point() +
  facet_wrap(~team) +
  scale_alpha_discrete(range = c(.25, 1)) +
  guides(alpha=FALSE) +
  labs(x = "Game Number",
       y = "Cumulative xG Differential",
       caption = "@Null_HHockey, data from corsica.ca",
       title = "NHL",
       subtitle = "All Situations")

