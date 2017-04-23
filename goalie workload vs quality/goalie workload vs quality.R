library(tidyverse)
library(viridis)

setwd("~/github folder/Random-Hockey-Charts")

theme_set(theme_bw())

df_5v5 <- read_csv("1617 goalie playoffs 5v5.csv")
df_5v5$sit <- "5v5"

df_4v5 <- read_csv("1617 goalie playoffs 4v5.csv")
df_4v5$sit <- "Shorthanded"

df_3v5 <- read_csv("1617 goalie playoffs 3v5.csv")
df_3v5$sit <- "Shorthanded"

df_shorthanded <- rbind(df_4v5, df_3v5)
colnames(df_shorthanded) <- tolower(colnames(df_shorthanded))

df_shorthanded <- df_shorthanded %>% 
  select(team, player, sit, toi, ca, xga) %>% 
  group_by(team, player) %>% 
  summarize(toi = sum(toi),
         ca = sum(ca),
         xga = sum(xga)) %>% 
  mutate(ca60 = ca/toi * 60,
         xga60 = xga/toi * 60)

colnames(df_5v5) <- tolower(colnames(df_5v5))


df_5v5 <- df_5v5 %>% 
  select(team, player, toi, ca, xga) %>% 
  mutate(ca60 = ca/toi * 60,
         xga60 = xga/toi * 60) %>%
  filter(toi > 60)

matchups = data.frame(series = c("WSH vs. TOR",
                                 "PIT vs. CBJ",
                                 "NYR vs. MTL",
                                 "BOS vs. OTT",
                                 "SJS vs. EDM",
                                 "CHI vs. NSH",
                                 "MIN vs. STL",
                                 "ANA vs. CGY"),
                      team1 = c("WSH",
                                "PIT",
                                "NYR",
                                "BOS",
                                "S.J",
                                "CHI",
                                "MIN",
                                "ANA"),
                      team2 = c("TOR",
                                "CBJ",
                                "MTL",
                                "OTT",
                                "EDM",
                                "NSH",
                                "STL",
                                "CGY"))

matchups <- matchups %>% 
  mutate_all(as.character) %>% 
  gather(order, team, -series)

df_5v5 <- df_5v5 %>% 
  left_join(matchups)

df_shorthanded <- df_shorthanded %>% 
  left_join(matchups)

df_shorthanded <- df_shorthanded %>% 
  filter(toi > 10)


ggplot(df_5v5, aes(xga60, ca60, label = player, fill = series)) +
  geom_label(size = 3) +
  labs(x = "Expected Goals Against Per Hour",
       y = "Shots Against Per Hour (Corsi)",
       title = "Goalie Workload vs. Shot Quality Against",
       subtitle = "2016-2017 NHL Playoffs, 5v5 Play",
       caption = "@Null_HHockey, Data from Corsi.ca") +
  guides(fill = guide_legend(title = "Series"))

ggplot(df_shorthanded, aes(xga60, ca60, label = player, fill = series)) +
  geom_label(size = 3) +
  labs(x = "Expected Goals Against Per Hour",
       y = "Shots Against Per Hour (Corsi)",
       title = "Goalie Workload vs. Shot Quality Against",
       subtitle = "2016-2017 NHL Playoffs, Shorthanded Play",
       caption = "@Null_HHockey, Data from Corsi.ca") +
  guides(fill = guide_legend(title = "Series"))

?scale_fill_viridis
