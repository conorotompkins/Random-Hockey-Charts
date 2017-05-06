install.packages("viridis")

library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df_raw <- read_csv("16_17 playoffs running charts 4_28.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

teams <- df_raw %>%
  group_by(team) %>% 
  mutate(team_toi_sd = sd(toi)) %>% 
  arrange(team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()


df_player <- df_raw %>% 
  mutate(team = factor(team, levels = teams)) %>% 
  arrange(team, player, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  group_by(player, team) %>% 
  mutate(toi_cum = cumsum(toi),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                       "F", "D")) %>% 
  select(team, player, game_number, position, toi, toi_cum) %>% 
  group_by(player, team) %>% 
  mutate(toi_sum = max(toi_cum)) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(toi_team_sum = sum(toi)) %>% 
  ungroup() %>% 
  mutate(toi_pct_team = round(toi_sum / toi_team_sum, digits = 3))


plot_player_league <- df_player %>% 
  ggplot(aes(game_number, toi_cum, color = position, group = player)) +
  geom_vline(xintercept = c(1:7), alpha = .25) +
  geom_line()

plot_player_league

plot_karlsson <- df_player %>% 
  mutate(is_karlsson = player == "ERIK.KARLSSON") %>% 
  ggplot(aes(game_number, toi_cum, color = position, group = player, alpha = is_karlsson)) +
  geom_vline(xintercept = c(1:7), alpha = .25) +
  geom_line()
plot_karlsson
  
plot_player <- df_player %>% 
  #filter(team == "PIT") %>% 
    ggplot(aes(game_number, toi_cum, group = player, color = position, size = toi_pct_team, alpha = toi_pct_team)) +
      geom_vline(xintercept = c(1:7), alpha = .25) +
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

df_pos <- df_raw %>% 
  mutate(team = factor(team, levels = teams),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  select(team, game_number, position, toi) %>% 
  group_by(team, game_number, position) %>% 
  summarize(toi_sd = sd(toi))

plot_position <- df_pos %>% 
  #filter(team == "PIT") %>% 
  ggplot(aes(game_number, toi_sd, color = position)) +
  geom_vline(xintercept = c(1:7), alpha = .25) +
  geom_line(size = 2) +
  facet_wrap(~team) +
  labs(title = "Roster Time On Ice Allocation Consistency",
       subtitle = "2016-17 NHL Playoffs, All Situations",
       x = "Game Number",
       y = "Standard Deviation of Time on Ice",
       caption = "A higher standard deviation indicates greater variance in time on ice @Null_HHockey, data from http://www.corsica.hockey/") +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0))
plot_position
?theme
