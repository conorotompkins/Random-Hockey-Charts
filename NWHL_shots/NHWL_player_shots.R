library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/NWHL_shots")

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

#http://www.nwhl.zone/stats/league_instance/46941?order_by=hksgm&order_dir=desc&stat_tab=ice_hockey_skater&subseason=327125
#copy each page of the stats leaderboard and run the corresponding line of code below

page_1 <- read.delim("clipboard")
page_2 <- read.delim("clipboard")
page_3 <- read.delim("clipboard")

#bind the pages together
my_files <- list(page_1 = page_1,
                 page_2 = page_2,
                 page_3 = page_3)


#clean up column anmes and select the columns we need
my_data <- bind_rows(my_files) %>%
  rename(name = `NAME`,
         team = `TEAM`) %>%
  select(team, name, SOG)

#save data
write_csv(my_data, "NWHL_shots_data.csv")

my_data <- read_csv("NWHL_shots_data.csv")


#create helper data for sorting the teams
team_helper <- my_data %>%
  group_by(team) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(-SOG) %>%
  ungroup() %>%
  select(team) %>%
  unique()

#create helper data for sorting the players
name_helper <- my_data %>%
  group_by(team, name) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(team, SOG) %>%
  ungroup() %>%
  select(name) %>%
  unique()

#create df for the graph  
df <- my_data %>%
  mutate(team = factor(team, levels = team_helper$team),
         name = factor(name, levels = name_helper$name)) %>%
  group_by(team, name) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(team, -SOG) %>%
  mutate(team_percent = round(SOG / sum(SOG), 2),
         position = cumsum(SOG) - (.5 * SOG))


#create df for colors and full team names
colors <- data.frame(team = unique(df$team),
                     fill = c("#23b14d",
                              "#fdb927",
                              "#6bb1e1",
                              "#c32036"),
                     color = c("#0564a8",
                               "#2f2f2f",
                               "#231f20",
                               "#c32036"),
                     full_team_name = c("Conneticut Whale",
                                        "Boston Pride",
                                        "New York Riveters",
                                        "Buffalo Beauts"))
colors <- colors %>%
  mutate(team = as.character(team),
         fill = as.character(fill),
         color = as.character(color))

#join them together
df <- df %>%
  left_join(colors) %>%
  ungroup %>%
  mutate(team = factor(team, levels = team_helper$team),
         fill = factor(fill, levels = colors$fill),
         color = factor(color, levels = colors$color),
         full_team_name = factor(full_team_name, levels = colors$full_team_name))


#create graph
ggplot(df, aes(full_team_name, SOG)) +
  geom_col(aes(alpha = (team_percent * 3), 
               fill = full_team_name),
           color = "black") +
  geom_text(data = df, aes(full_team_name, position, 
                                  label=ifelse(team_percent >= 0.05, paste0(name, ": ", sprintf("%.0f", team_percent*100),"%"),"")), size = 4) +
  labs(x = NULL,
       y = "Shots On Goal",
       title = "NWHL Player Shots",
       subtitle = "As of 2/1/2017",
       caption = "Players with less than %5 of their team's shots not annotated \n @Null_HHockey") +
  guides(fill = FALSE,
         alpha = FALSE,
         color = FALSE) +
  #scale_fill_viridis(discrete = TRUE) +
  #scale_color_manual(values = c("Conneticut Whale" = "#0564a8",
  #                              "Boston Pride" = "#2f2f2f",
  #                              "Buffalo Beauts" = "#231f20",
  #                              "New York Riveters" = "#111f47")) +
  scale_fill_manual(values =  c("Conneticut Whale" = "#23b14d",
                                "Boston Pride" = "#fdb927",
                                "Buffalo Beauts" = "#6bb1e1",
                                "New York Riveters"  = "#c32036")) +
  #scale_color_manual(values = colors$color, 
                     #labels = colors$team) +
  #scale_fill_manual(values = colors$fill, 
                     #labels = colors$team) +
  #scale_color_discrete(values = colors$color) +
  #scale_fill_discrete(values = colors$fill) +
  scale_x_discrete(expand = c(0, .5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 1))

ggsave("NWHL Shots Bar Plot team colors.png", width = 16, height = 9)
