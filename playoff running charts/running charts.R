library(tidyverse)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df <- read_csv("16_17 playoffs running charts 4_27.csv")

colnames(df) <- tolower(colnames(df))

df <- df %>% 
  arrange(team, player, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  group_by(player, team) %>% 
  mutate(toi_cum = cumsum(toi),
         pos = case_when(.$position == "D" ~ "D",
                              .$position == "G" ~ "G",
                              .$position != "D" ~ "F")
         ) %>% 
  select(team, player, position, toi_cum)
?case_when

df %>% 
  #filter(team == "PIT") %>% 
    ggplot(aes(game_number, toi_cum, group = player)) +
      geom_line() +
      facet_wrap(~team)
