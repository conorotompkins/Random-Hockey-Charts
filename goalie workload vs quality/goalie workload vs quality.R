library(tidyverse)
library(viridis)
library(lubridate)

setwd("/github folder/Random-Hockey-Charts")

theme_set(theme_bw())

individual_5v5 <- read_csv("individual goalie playoffs 5v5.csv")
individual_5v5$sit <- "5v5"
colnames(individual_5v5) <- tolower(colnames(individual_5v5))

individual_4v5 <- read_csv("individual goalie playoffs 4v5.csv")
individual_4v5$sit <- "4v5"
colnames(individual_4v5) <- tolower(colnames(individual_4v5))

individual_3v5 <- read_csv("individual goalie playoffs 3v5.csv")
individual_3v5$sit <- "3v5"
colnames(individual_3v5) <- tolower(colnames(individual_3v5))

individual_shorthanded <- bind_rows(individual_3v5, individual_4v5)
individual_shorthanded <- individual_shorthanded %>% 
  replace_na(list(ca60 = 0, xga60 = 0))

df_individual <- bind_rows(individual_5v5, individual_shorthanded)
df_individual$sit[df_individual$sit == "3v5" | df_individual$sit == "4v5"] <- "Shorthanded"

#df_5v5 <- read_csv("1617 goalie playoffs 5v5.csv")
#df_5v5$sit <- "5v5"

#df_4v5 <- read_csv("1617 goalie playoffs 4v5.csv")
#df_4v5$sit <- "Shorthanded"

#df_3v5 <- read_csv("1617 goalie playoffs 3v5.csv")
#df_3v5$sit <- "Shorthanded"
#
#df_shorthanded <- rbind(df_4v5, df_3v5)
#colnames(df_shorthanded) <- tolower(colnames(df_shorthanded))

#df_shorthanded <- df_shorthanded %>% 
#  select(team, player, sit, toi, ca, xga) %>% 
#  group_by(team, player) %>% 
#  summarize(toi = sum(toi),
#        ca = sum(ca),
#         xga = sum(xga)) %>% 
#  mutate(ca60 = ca/toi * 60,
#         xga60 = xga/toi * 60)

#colnames(df_5v5) <- tolower(colnames(df_5v5))


#df_5v5 <- df_5v5 %>% 
#  select(team, player, toi, ca, xga) %>% 
#  mutate(ca60 = ca/toi * 60,
#        xga60 = xga/toi * 60) %>%
#  filter(toi > 60)



matchups = data.frame(round = c(rep.int(1, 8), rep.int(2, 4)),
                      series = c("WSH vs. TOR",
                                 "PIT vs. CBJ",
                                 "NYR vs. MTL",
                                 "BOS vs. OTT",
                                 "SJS vs. EDM",
                                 "CHI vs. NSH",
                                 "MIN vs. STL",
                                 "ANA vs. CGY",
                                 "NSH vs. STL",
                                 "ANA vs. EDM",
                                 "NYR vs. OTT",
                                 "WSH vs. PIT"),
                      team1 = c("WSH",
                                "PIT",
                                "NYR",
                                "BOS",
                                "S.J",
                                "CHI",
                                "MIN",
                                "ANA",
                                "NSH",
                                "ANA",
                                "NYR",
                                "WSH"),
                      team2 = c("TOR",
                                "CBJ",
                                "MTL",
                                "OTT",
                                "EDM",
                                "NSH",
                                "STL",
                                "CGY",
                                "STL",
                                "EDM",
                                "OTT",
                                "PIT"))
dates <- data_frame(round = c(rep(1, 12), rep(2, 18)),
                    date = seq(ymd("2017-04-12"), ymd("2017-05-11"), by = "days"))

matchups <- matchups %>% 
  mutate_at(vars(series, team1, team2), as.character) %>% 
  gather(order, team, -(c(round, series))) %>% 
  arrange(round, series)

df_individual <- df_individual %>% 
  left_join(dates) %>% 
  left_join(matchups)

df_individual <- df_individual %>% 
  select(sit, team, date, player, series, round, toi, ca60, xga60) %>% 
  group_by(sit, team, round, series, player) %>% 
  summarize(xga60 = mean(xga60),
            ca60 = mean(ca60),
            toi = sum(toi))



#df_5v5 <- df_5v5 %>% 
#  left_join(matchups)

#df_shorthanded <- df_shorthanded %>% 
#  left_join(matchups)

#df_shorthanded <- df_shorthanded %>% 
#  filter(toi > 10)


#ggplot(df_5v5, aes(xga60, ca60, label = player, fill = series)) +
#  geom_label(size = 3) +
#  facet_wrap(~round) +
#  labs(x = "Expected Goals Against Per Hour",
#       y = "Shots Against Per Hour (Corsi)",
#       title = "Goalie Workload vs. Shot Quality Against",
#       subtitle = "2016-2017 NHL Playoffs, 5v5 Play",
#       caption = "@Null_HHockey, Data from Corsi.ca") +
#  guides(fill = guide_legend(title = "Series"))

#ggplot(df_shorthanded, aes(xga60, ca60, label = player, fill = series)) +
#  geom_label(size = 3) +
#  labs(x = "Expected Goals Against Per Hour",
#       y = "Shots Against Per Hour (Corsi)",
#       title = "Goalie Workload vs. Shot Quality Against",
#       subtitle = "2016-2017 NHL Playoffs, Shorthanded Play",
#       caption = "@Null_HHockey, Data from Corsi.ca") +
#  guides(fill = guide_legend(title = "Series"))


df_individual %>% 
  filter(sit == "5v5") %>% 
  filter(toi > 40) %>% 
ggplot(aes(xga60, ca60, label = player, fill = series)) +
  geom_label(size = 3) +
  facet_wrap(sit~paste("Round", round)) +
  labs(x = "Expected Goals Against Per Hour",
       y = "Shots Against Per Hour (Corsi)",
       title = "Goalie Workload vs. Shot Quality Against",
       subtitle = paste0("2016-2017 NHL Playoffs, ", individual_5v5$sit, " Play"),
       caption = "@Null_HHockey, Data from Corsi.ca") +
  guides(fill = guide_legend(title = "Series"))

df_individual %>% 
  filter(sit == "Shorthanded") %>% 
  filter(toi > 4) %>% 
  ggplot(aes(xga60, ca60, label = player, fill = series)) +
  geom_label(size = 3) +
  facet_wrap(sit~paste("Round", round)) +
  labs(x = "Expected Goals Against Per Hour",
       y = "Shots Against Per Hour (Corsi)",
       title = "Goalie Workload vs. Shot Quality Against",
       subtitle = paste0("2016-2017 NHL Playoffs, ", individual_5v5$sit, " Play"),
       caption = "@Null_HHockey, Data from Corsi.ca") +
  guides(fill = guide_legend(title = "Series"))
