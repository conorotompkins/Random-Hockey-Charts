install.packages("viridis")

library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df_raw <- read_csv("16_17 playoffs running charts 4_28.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

df_player <- df_raw %>% 
  arrange(team, player, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  group_by(player, team) %>% 
  mutate(toi_cum = cumsum(toi),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                       "F", "D")) %>% 
  select(team, player, game_number, position, toi_cum) %>% 
  group_by(player, team) %>% 
  mutate(toi_sum = max(toi_cum)) %>% 
  ungroup()


df_player %>% 
  #filter(team == "PIT") %>% 
    ggplot(aes(game_number, toi_cum, group = player, color = position)) +
      geom_line() +
      facet_wrap(~team)
      #scale_color_viridis(discrete = TRUE)


df_pos <- df_raw %>% 
  mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  select(team, game_number, position, player, toi) %>% 
  group_by(player, team) %>% 
  mutate(toi_cum = cumsum(toi)) %>% 
  ungroup()

df_pos %>% 
  #filter(team == "PIT") %>% 
  ggplot(aes(game_number, toi_cum, group = position, color = position)) +
  geom_jitter(width = .1, alpha = .5) +
  geom_smooth() +
  facet_wrap(~team) +
  labs(title = "Cumulative Time On Ice",
       subtitle = "2016-17 NHL Playoffs, All Situations",
       x = "Game Number",
       y = "Cumulative Time On Ice") +
  scale_x_continuous(breaks = c(1:7))
#scale_color_viridis(discrete = TRUE)
warnings()
?geom_jitter
