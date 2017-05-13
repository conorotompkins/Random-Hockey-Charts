library(tidyverse)
library(viridis)
library(lubridate)
library(viridis)

#setwd("~/github folder/Random-Hockey-Charts")

#set theme
theme_set(theme_bw())

#read data into R
df_raw <- read_csv("player per season data 5v5.csv")

#change column names to lower case
colnames(df_raw) <- tolower(colnames(df_raw))

#modify df
df <- df_raw %>% 
  mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                            "Forward", "Defense")) %>% #chnage positions to Forward and Defense
  arrange(player, season) %>% #arrange by player and season
  group_by(player) %>% #group by player
  mutate(toi_prev = lag(toi)) %>% #create a new column for the previous season's TOI
  select(season, player, position, toi, toi_prev) #filter out columns we don't need

df #view the df. if the player didn't have a previous season of TOI, the value for toi_prev is NA

(plot <- df %>% 
  na.omit() %>% #remove rows with an NA value
  ggplot(aes(toi_prev, toi, fill = position, color = position)) +
  geom_point(alpha = .3) +
  geom_smooth() +
  facet_wrap(~position))

ggsave("toi autocorrelation plot.png")

#run a correlation
df_clean <- df %>% 
  na.omit()
cor(df_clean$toi, df_clean$toi_prev) ^ 2
