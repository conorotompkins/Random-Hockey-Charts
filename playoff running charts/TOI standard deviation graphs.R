library(tidyverse)
library(viridis)
library(lubridate)
library(viridis)

setwd("~/github folder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df_raw <- read_csv("playoff running charts/16_17 playoffs running charts 5_27.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

matchups = data.frame(round = c(rep.int(1, 8), rep.int(2, 4), rep.int(3, 2)),
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
                                 "WSH vs. PIT",
                                 "PIT vs. OTT",
                                 "NSH vs. ANA"),
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
                                "WSH",
                                "PIT",
                                "NSH"),
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
                                "PIT", 
                                "OTT",
                                "ANA"))


round1 <- data_frame(round = c(rep(1, 12)),
                     date = seq(ymd("2017-04-12"), ymd("2017-04-23"), by = "days"))
round2 <- data_frame(round = c(rep(2, 18)),
                     date = seq(ymd("2017-04-24"), ymd("2017-05-11"), by = "days"))
round3 <- data_frame(round = c(rep(3, 14)),
                     date = seq(ymd("2017-05-12"), ymd("2017-05-25"), by = "days"))
dates <- bind_rows(round1, round2, round3)

df_raw <- df_raw %>% 
  mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                            "Forward", "Defense")) 

matchups <- matchups %>% 
  mutate_at(vars(series, team1, team2), as.character) %>% 
  gather(order, team, -(c(round, series))) %>% 
  arrange(round, series)

teams <- df_raw %>%
  group_by(team) %>% 
  mutate(team_toi_sd = sd(toi)) %>% 
  arrange(team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

teams_pos <- df_raw %>%
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date),
         team_toi_sd = sd(toi),
         gp = max(game_number)) %>% 
  arrange(desc(gp), team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()


league_sd <- df_raw %>% 
  group_by(position) %>% 
  summarize(league_toi_sd = sd(toi))

eliminated <- list(team = c("BOS",
                            "CHI",
                            "MIN",
                            "CGY",
                            "SJS",
                            "MTL",
                            "CBJ",
                            "TOR",
                            "OTT",
                            "ANA"))

last_date <- max(df_raw$date)
my_subtitle <- paste("2016-17 NHL Playoffs, All Situations, Updated:", last_date)

df_pos <- df_raw %>% 
  mutate(team = factor(team, levels = teams_pos)) %>% 
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  select(team, game_number, position, toi) %>% 
  group_by(team, game_number, position) %>% 
  summarize(toi_sd = sd(toi)) %>% 
  mutate(eliminated = team %in% eliminated$team) %>% 
  left_join(league_sd, by = c("position"))

line_plot_position <- df_pos %>% 
  ggplot(aes(game_number, toi_sd, color = position)) +
  geom_hline(aes(color = position, yintercept = league_toi_sd)) +
  geom_vline(xintercept = c(1:max(df_pos$game_number)), alpha = .25) +
  geom_line(size = 2) +
  scale_x_continuous(breaks = 1:max(df_pos$game_number)) +
  facet_wrap(~team) +
  labs(title = "Roster Time On Ice Allocation Consistency",
       subtitle = my_subtitle,
       x = "Game Number",
       y = "Standard Deviation of Time on Ice",
       caption = "@Null_HHockey, data from http://www.corsica.hockey/") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))
line_plot_position
ggsave("line_plot_position_standard_deviation.png")


teams_boxplot_F <- df_raw %>%
  filter(position == "Forward") %>% 
  group_by(team) %>% 
  summarize(team_toi_median = median(toi)) %>% 
  arrange(team_toi_median) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

teams_boxplot_D <- df_raw %>%
  filter(position == "Defense") %>% 
  group_by(team) %>% 
  summarize(team_toi_median = median(toi)) %>% 
  arrange(team_toi_median) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

boxplot_position_F <- df_raw %>%
  filter(position == "Forward") %>% 
  mutate(team = factor(team, levels = teams_boxplot_F)) %>% 
  group_by(team, position, date) %>% 
  summarize(toi_sd = sd(toi))

boxplot_position_D <- df_raw %>%
  filter(position == "Defense") %>% 
  mutate(team = factor(team, levels = teams_boxplot_D)) %>% 
  group_by(team, position, date) %>% 
  summarize(toi_sd = sd(toi))
  
team_boxplot_F_plot <- boxplot_position_F %>% 
  ggplot(aes(team, toi_sd)) +
  geom_jitter(alpha = .2,
              width = .2) +
  geom_boxplot(alpha = .3,
               fill = "blue") +
  facet_wrap(~position) +
  labs(x = NULL,
       y = "Standard Deviation of TOI per game",
       title = "Roster Time On Ice Allocation Consistency",
       subtitle = my_subtitle,
       caption = "@Null_HHockey, data from http://www.corsica.hockey/")
team_boxplot_F_plot
ggsave("forward_team_boxplot.png")

team_boxplot_D_plot <- boxplot_position_D %>% 
  ggplot(aes(team, toi_sd)) +
  geom_jitter(alpha = .2,
              width = .2) +
  geom_boxplot(alpha = .3,
               fill = "red") +
  facet_wrap(~position) +
  labs(x = NULL,
       y = "Standard Deviation of TOI per game",
       title = "Roster Time On Ice Allocation Consistency",
       subtitle = my_subtitle,
       caption = "@Null_HHockey, data from http://www.corsica.hockey/")
team_boxplot_D_plot
ggsave("defense_team_boxplot.png")

#team_position <- df_raw %>% 
#  mutate(team = factor(team, levels = teams),
#         position = if_else(position == "L" | position == "C" | position ==  "R",
#                            "Forward", "Defense")) %>% 
#  arrange(team, position, date) %>% 
#  select(team, position, toi) %>% 
#  group_by(team, position) %>% 
#  summarize(toi_sd = sd(toi)) %>% 
#  ungroup() %>% 
#  mutate(eliminated = team %in% eliminated$team) %>% 
#  arrange(position, toi_sd) %>% 
#  mutate(order = row_number())
  
#team_bar_position_plot <- team_position %>% 
#  ggplot(aes(order, toi_sd, fill = position)) +
#  geom_bar(stat = "identity", color = "black") +
#  scale_x_continuous(labels = team_position$team,
#                     breaks = team_position$order) +
#  facet_wrap(~position,
#             scales = "free") +
#  coord_flip()
#team_bar_position_plot

team_position <- df_raw %>% 
  mutate(team = factor(team, levels = teams)) %>% 
  group_by(team, position) %>% 
  summarize(toi_sd = sd(toi))


team_scatter_position <- team_position %>% 
  select(team, position, toi_sd) %>% 
  spread(key = position,
         value = toi_sd,
         sep = "_TOI_SD_") %>% 
  ggplot(aes(position_TOI_SD_Defense, position_TOI_SD_Forward, label = team)) +
  geom_label() +
  labs(x = "Standard Deviation of Defense TOI",
       y = "Standard Deviation of Forwards TOI",
       title = "Distribution of Forward and Defense Lines",
       subtitle = my_subtitle,
       caption = "@Null_HHockey, data from Corsica.Hockey.CA")
team_scatter_position
ggsave("team_scatter_position.png")

team_pos_date_df <- df_raw %>% 
  select(team, date, position, player, toi) %>% 
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup() %>% 
  left_join(dates) %>% 
  left_join(matchups) %>% 
  group_by(series) %>% 
  mutate(game_number_series = dense_rank(date)) %>% 
  ungroup()


selected_series <-  "PIT vs. CBJ" 
team_pos_date_df %>% 
  filter(series == selected_series) %>% 
  ggplot(aes(game_number_series, toi, color = position, group = date)) +
  geom_jitter(width = .1,
              alpha = .7) +
  geom_violin(alpha = .3) +
  #look into draw_quantiles for the violins
  facet_wrap(team~position) +
  scale_x_continuous(breaks = c(1:7)) + 
  #scale_color_viridis(discrete = TRUE) +
  #scale_fill_viridis(discrete = TRUE) +
  labs(title = selected_series,
       subtitle = my_subtitle,
       x = "Game Number",
       y = "Time On Ice") +
  theme(panel.grid.minor = element_blank())
ggsave(paste0(selected_series, ".png"))

#trying to get scale_x_continuous to respond to the data
c(1:team_pos_date_df %>% 
    filter(series == selected_series) %>% 
    max(game_number_series))
  
    summarize(games = max(game_number_series)) %>% 
    select(games) %>% 
    unlist())
