library(tidyverse)
library(viridis)

setwd("C:/Users/conor/githubfolder/Random-Hockey-Charts/NWHL_shots")

source("https://raw.githubusercontent.com/conorotompkins/AdjGSAA/master/graphs/theme_nhh.R")
theme_set(theme_nhh())

#http://www.nwhl.zone/stats/league_instance/46941?order_by=hksgm&order_dir=desc&stat_tab=ice_hockey_skater&subseason=327125

page_1 <- read.delim("clipboard")
page_2 <- read.delim("clipboard")
page_3 <- read.delim("clipboard")
my_files <- list(page_1 = page_1,
                 page_2 = page_2,
                 page_3 = page_3)


my_data <- bind_rows(my_files) %>%
  rename(name = `NAME`,
         team = `TEAM`) %>%
  select(team, name, SOG)

write_csv(my_data, "NWHL_shots_data.csv")

my_data <- read_csv("NWHL_shots_data.csv")



team_helper <- my_data %>%
  group_by(team) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(-SOG) %>%
  ungroup() %>%
  select(team) %>%
  unique()

name_helper <- my_data %>%
  group_by(team, name) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(team, SOG) %>%
  ungroup() %>%
  select(name) %>%
  unique()
  
df <- my_data %>%
  mutate(team = factor(team, levels = team_helper$team),
         name = factor(name, levels = name_helper$name)) %>%
  group_by(team, name) %>%
  summarize(SOG = sum(SOG)) %>%
  arrange(team, -SOG) %>%
  mutate(team_percent = round(SOG / sum(SOG), 2),
         position = cumsum(SOG) - (.5 * SOG))


unique(df$team)
colors <- data.frame(team = unique(df$team),
                     fill = c("#23b14d",
                              "#fdb927",
                              "#6bb1e1",
                              "#111f47"),
                     color = c("#0564a8",
                               "#2f2f2f",
                               "#231f20",
                               "#c32036"))
colors <- colors %>%
  mutate(team = as.character(team),
         fill = as.character(fill),
         color = as.character(color))


df <- df %>%
  left_join(colors) %>%
  ungroup %>%
  mutate(team = factor(team, levels = team_helper$team))



ggplot(df, aes(team, SOG)) +
  geom_col(aes(alpha = (team_percent + .5), 
               fill = team),
           color = "black") +
  geom_text(data = df, aes(team, position, 
                                  label=ifelse(team_percent >= 0.05, paste0(name, ": ", sprintf("%.0f", team_percent*100),"%"),"")), size = 4) +
  labs(x = "Team",
       y = "Shots On Goal",
       title = "NWHL Shots",
       caption = "Players with less than %5 of their team's shots not annotated \n @Null_HHockey") +
  guides(fill = FALSE,
         alpha = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  #scale_color_manual(values = rev(c("#0564a8",
  #                              "#2f2f2f",
  #                              "#231f20",
  #                              "#c32036"))) +
  #scale_fill_manual(values =  rev(c("#23b14d",
  #                              "#fdb927",
   #                             "#6bb1e1",
    #                            "#111f47")))
  #scale_color_manual(values = colors$color, 
                     #labels = colors$team) +
  #scale_fill_manual(values = colors$fill, 
                     #labels = colors$team) +
  #scale_color_discrete(values = colors$color) +
  #scale_fill_discrete(values = colors$fill) +
  scale_x_discrete(expand = c(0, .5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 350)) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text(hjust = 1))

ggsave("NWHL Shots Bar Plot.png", width = 16, height = 9)
  

category1_helper <- my_data_df %>%
  filter(!(category2 %in% category_2_exclude)) %>%
  group_by(category1) %>%
  summarize(amount = sum(amount)) %>%
  arrange(-amount) %>%
  ungroup() %>%
  select(category1) %>%
  unique()

category1_helper <- category1_helper$category1


category2_helper <- my_data_df %>%
  filter(!(category2 %in% category_2_exclude)) %>%
  group_by(category1, category2) %>%
  summarize(amount = sum(amount)) %>%
  arrange(category1, amount) %>%
  ungroup() %>%
  select(category2) %>%
  unique()

category2_helper <- category2_helper$category2


bar_plot1 <- my_data_df %>%
  filter(!(category2 %in% category_2_exclude)) %>%
  mutate(category1 = factor(category1, levels = category1_helper),
         category2 = factor(category2, levels = category2_helper)) %>%
  group_by(category1, category2) %>%
  summarize(amount = sum(amount)) %>%
  arrange(category1, -amount) %>%
  mutate(category1_percent = round(amount / sum(amount), 2),
         position = cumsum(amount) - (.5 * amount)) %>%
  filter(!is.na(category2))

ggplot(bar_plot1, aes(category1, amount)) +
  geom_col(aes(fill = category2, alpha = category1_percent), color = "black") +
  geom_text(data = bar_plot1, aes(category1, position, 
                                  label=ifelse(category1_percent >= 0.05, paste0(category2, ": ", sprintf("%.0f", category1_percent*100),"%"),"")), size = 3) +
  scale_fill_viridis(discrete = TRUE) +
  coord_flip() +
  labs(y = "Amount",
       x = "Main Category",
       title = "Spending Breakdown",
       subtitle = paste0(min(my_data_df$trans_date), " to ", max(my_data_df$trans_date)),
       caption = paste("Categories excluded:", paste(category_2_exclude, collapse = ", "))) +
  guides(fill = FALSE,
         alpha = FALSE) +
  scale_x_discrete(expand = c(0, .5)) +
  scale_y_continuous(expand = c(.05, 0), labels = dollar) +
  theme(panel.grid = element_blank(),
        plot.caption = element_text())