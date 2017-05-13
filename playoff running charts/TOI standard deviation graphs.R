library(tidyverse)
library(viridis)

setwd("~/github folder/Random-Hockey-Charts/playoff running charts")

theme_set(theme_bw())

df_raw <- read_csv("16_17 playoffs running charts 5_12.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

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
  mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  group_by(position) %>% 
  summarize(league_toi_sd = sd(toi))

eliminated <- list(team = c("BOS",
                            "CHI",
                            "MIN",
                            "CGY",
                            "SJS",
                            "MTL",
                            "CBJ",
                            "TOR"))

df_pos <- df_raw %>% 
  mutate(team = factor(team, levels = teams_pos),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
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
  facet_wrap(~team) +
  labs(title = "Roster Time On Ice Allocation Consistency",
       subtitle = "2016-17 NHL Playoffs, All Situations",
       x = "Game Number",
       y = "Standard Deviation of Time on Ice",
       caption = "@Null_HHockey, data from http://www.corsica.hockey/") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))
line_plot_position



teams_boxplot <- df_raw %>%
  group_by(team) %>% 
  summarize(team_toi_sd = sd(toi)) %>% 
  arrange(team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

boxplot_position <- df_raw %>%
  mutate(team = factor(team, levels = teams_boxplot),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  group_by(team, position, date) %>% 
  summarize(toi_sd = sd(toi))
  
team_position_boxplot <- boxplot_position %>% 
  ggplot(aes(team, toi_sd, fill = position)) +
  geom_jitter(aes(color = position), 
              alpha = .2,
              width = .2) +
  geom_boxplot(alpha = .3) +
  facet_wrap(~position)
team_position_boxplot

team_position <- df_raw %>% 
  mutate(team = factor(team, levels = teams),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D")) %>% 
  arrange(team, position, date) %>% 
  select(team, position, toi) %>% 
  group_by(team, position) %>% 
  summarize(toi_sd = sd(toi)) %>% 
  ungroup() %>% 
  mutate(eliminated = team %in% eliminated$team) %>% 
  arrange(position, toi_sd) %>% 
  mutate(order = row_number())
  
team_bar_position_plot <- team_position %>% 
  ggplot(aes(order, toi_sd, fill = position)) +
  geom_bar(stat = "identity", color = "black") +
  scale_x_continuous(labels = team_position$team,
                     breaks = team_position$order) +
  facet_wrap(~position,
             scales = "free") +
  coord_flip()
team_bar_position_plot
?scale_x_discrete

team_scatter_position <- team_position %>% 
  select(team, position, toi_sd) %>% 
  spread(key = position,
         value = toi_sd,
         sep = "_TOI_SD_") %>% 
  ggplot(aes(position_TOI_SD_D, position_TOI_SD_F, label = team)) +
  geom_label() +
  geom_smooth() +
  #annotate(geom = "text",
  #         x = 6,
  #         y = 4.4,
  #         label = "Higher --> More Variance") +
  labs(x = "Standard Deviation of Defense TOI",
       y = "Standard Deviation of Forwards TOI",
       title = "Distribution of Forward and Defense Lines",
       subtitle = "2016-17 NHL Playoffs, All-Situations Play",
       caption = "@Null_HHockey, data from Corsica.Hockey.CA")
team_scatter_position

team_pos_date_df <- df_raw %>% 
  select(team, date, position, player, toi) %>% 
mutate(position = if_else(position == "L" | position == "C" | position ==  "R",
                          "F", "D")) %>% 
  arrange(team, position, date) %>% 
  group_by(team) %>% 
  mutate(game_number = dense_rank(date)) %>% 
  ungroup()
  
team_pos_date_df %>% 
  filter(team == "PIT") %>% 
  ggplot(aes(game_number, toi, color = position, group = game_number)) +
  geom_point() +
  geom_boxplot(outlier.color = NULL) +
  facet_wrap(~position) +
  scale_x_continuous(breaks = c(1:max(team_pos_date_df %>% 
                                        filter(team == "PIT") %>% select(game_number))))

?geom_boxplot
