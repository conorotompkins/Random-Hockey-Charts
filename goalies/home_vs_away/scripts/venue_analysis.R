library(tidyverse)
library(gghighlight)
library(ggrepel)

theme_set(theme_bw())

df_home <- read_csv("goalies/home_vs_away/data/data_home.csv") %>% 
  mutate(venue = "home")
df_away <- read_csv("goalies/home_vs_away/data/data_away.csv") %>% 
  mutate(venue = "away")

df <- bind_rows(df_home, df_away)

colnames(df) <- tolower(colnames(df))

colnames(df) <- str_replace(colnames(df), "%", "_per")

df_venue <- df %>% 
  filter(sa >= 800) %>% 
  select(player, season, team, venue, dsv_per) %>% 
  spread(venue, dsv_per) %>% 
  mutate(diff = home - away,
         key = str_c(player, season, sep = ", "))


gghighlight_point(data = df_venue, aes(home, away), label_key = key, predicate = abs(diff) > 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Save % - Expected Sv%, 2007-08 to 2017-18",
       x = "Home",
       y = "Away",
       subtitle = "Minimum 800 shots faced, All Situations",
       caption = "@Null_HHockey, data from corsica.hockey")


df_venue_1718 <- df %>% 
  filter(sa >= 100,
         season == "2017-2018") %>% 
  select(player, season, team, venue, dsv_per) %>% 
  spread(venue, dsv_per) %>% 
  mutate(diff = home - away)

gghighlight_point(data = df_venue_1718, aes(home, away), label_key = player, predicate = abs(diff) > 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_abline(intercept = 0, slope = 1) +
  labs(title = "Save % - Expected Sv%",
       x = "Home",
       y = "Away",
       subtitle = "Minimum 100 shots faced, All Situations 2017-2018",
       caption = "@Null_HHockey, data from corsica.hockey")

df_venue_1718 %>% 
  #filter(str_detect(team, "PIT")) %>% 
  ggplot(aes(home, away, label = player)) +
  geom_point() +
  geom_label_repel(size = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  #geom_abline(intercept = 0, slope = 1) +
  labs(title = "Save % - Expected Sv%",
       x = "Home",
       y = "Away",
       subtitle = "Minimum 100 shots faced, All Situations 2017-2018",
       caption = "@Null_HHockey, data from corsica.hockey") +
  annotate("text", x = c(-5, -5, 3.8, 4), y  = c(-5.5, 5, 5, -5.5), label = c("Nah", "Road Warrior", "Geography-agnostic", "Home cookin'"), 
           size = 4)
ggsave("goalies/home_vs_away/images/goalie_venue_201718.png")