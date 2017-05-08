library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/playoff running charts/")

theme_set(theme_bw())

df_raw <- read_csv("playoff running charts/16_17 playoffs running charts 5_7.csv")

colnames(df_raw) <- tolower(colnames(df_raw))

teams <- df_raw %>%
  group_by(team) %>% 
  mutate(team_toi_sd = sd(toi)) %>% 
  arrange(team_toi_sd) %>% 
  select(team) %>% 
  unique() %>% 
  unlist()

eliminated <- list(team = c("BOS",
                            "CHI",
                            "MIN",
                            "CGY",
                            "SJS",
                            "MTL",
                            "CBJ",
                            "TOR"))

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
  summarize(toi_sd = sd(toi)) %>% 
  mutate(eliminated = team %in% eliminated$team) %>% 
  arrange(desc(eliminated))

line_plot_position <- df_pos %>% 
  ggplot(aes(game_number, toi_sd, color = position)) +
  geom_vline(xintercept = c(1:max(df_pos$game_number)), alpha = .25) +
  geom_line(size = 2) +
  facet_wrap(~team) +
  labs(title = "Roster Time On Ice Allocation Consistency",
       subtitle = "2016-17 NHL Playoffs, All Situations",
       x = "Game Number",
       y = "Standard Deviation of Time on Ice",
       caption = "A higher standard deviation indicates greater variance in time on ice @Null_HHockey, data from http://www.corsica.hockey/") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))
line_plot_position

boxplot_position <- df_raw %>%
  mutate(team = factor(team, levels = teams),
         position = if_else(position == "L" | position == "C" | position ==  "R",
                            "F", "D"))
  
team_position_boxplot <- boxplot_position %>% 
  ggplot(aes(team, toi, fill = position)) +
  geom_boxplot()
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
  annotate(geom = "text",
           x = 6,
           y = 4.4,
           label = "Higher --> More Variance") +
  labs(x = "Standard Deviation of Defense TOI",
       y = "Standard Deviation of Forwards TOI",
       title = "Distribution of Forward and Defense Lines",
       subtitle = "2016-17 NHL Playoffs, All-Situations Play",
       caption = "@Null_HHockey, data from Corsica.Hockey.CA")
team_scatter_position
?spread
