library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df_raw <- read_csv("16_17 playoffs running charts 5_21.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

schedule <- df_raw %>% 
  select(team, date) %>% 
  group_by(team, date) %>%
  unique() %>% 
  arrange(team, date)


teams <- df_raw %>%
  group_by(team) %>% 
  mutate(team_toi_sd = sd(toi)) %>% 
  arrange(team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

player_position <- df_raw %>% 
  select(player, position) %>% 
  unique()


df_player <- df_raw %>%
  select(team, player, date, toi) %>% 
  group_by(team) %>% 
  right_join(schedule, by = c("date", "team")) %>% 
  complete(date, player) %>% 
  arrange(team, player, date) %>% 
  ungroup() %>% 
  replace_na(list(toi = 0)) %>% 
  left_join(player_position) %>% 
  group_by(team, player) %>% 
  mutate(toi_cum = cumsum(toi)) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  group_by(team) %>%
  mutate(gp_team = max(game_number)) %>% 
  arrange(desc(gp_team), team, player, date) %>% 
  ungroup() %>% 
  mutate(team = factor(team))

  
plot_player_league <- df_player %>% 
  ggplot(aes(game_number, toi_cum, color = position, group = player)) +
  geom_vline(xintercept = c(1:max(df_player$game_number)), alpha = .25) +
  geom_line() +
  facet_wrap(~position, ncol = 1)
plot_player_league

plot_karlsson <- df_player %>% 
  mutate(is_karlsson = player == "ERIK.KARLSSON") %>% 
  ggplot(aes(game_number, toi_cum, color = position, group = player, alpha = is_karlsson)) +
  geom_vline(xintercept = c(1:7), alpha = .25) +
  geom_line()
plot_karlsson
  
plot_player <- df_player %>% 
  #filter(team == "PIT") %>% 
    ggplot(aes(game_number, toi_cum, group = player, color = position)) +
      geom_vline(xintercept = c(1:max(df_player$game_number)), alpha = .25) +
      geom_line() +
      facet_wrap(~team) +
      scale_color_discrete(guide_colorbar(title = "Player Position")) +
      scale_size_continuous(range = c(.25, 1.25), guide = "none") +
      scale_alpha_continuous(range = c(.05, 1), guide = "none") +
      labs(title = "Cumulative Time on Ice Per Player",
       subtitle = "2016-17 NHL Playoffs, All Situations",
       x = "Game Number",
       y = "Cumulative Time on Ice",
       caption = "@Null_HHockey, data from http://www.corsica.hockey/") +
      theme(panel.grid.minor = element_blank())
      #scale_color_viridis(discrete = TRUE)
plot_player
