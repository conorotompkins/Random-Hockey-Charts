
library(tidyverse)
library(FFTrees)
library(intubate)

df <- read_csv("https://raw.githubusercontent.com/conorotompkins/NHL-Coaches/master/NHH_coach_data.csv") %>%
        mutate(season = as.character(season))


playoff_1617_data <- read.delim("clipboard") 

playoff_1617 <- playoff_1617_data %>%
        mutate(full_team_names = as.character(Team),
               playoff = TRUE,
               season = "20152016") %>%
        select(full_team_names, season, playoff)

test <- df %>%
        filter(season == "20152016") %>%
        select(season, full_team_names, team, CF60, CA60, OSh.per, OSv.per, FO.per, PN, PN.) %>%
        left_join(., playoff_1617, by = c("full_team_names", "season")) %>%
        group_by(team, season, playoff) %>%
        summarize(CF60 = mean(CF60, na.rm = TRUE),
                  CA60 = mean(CA60, na.rm = TRUE),
                  OSh.per = mean(OSh.per, na.rm = TRUE),
                  OSv.per = mean(OSv.per, na.rm = TRUE),
                  FO.per = mean(FO.per, na.rm = TRUE),
                  PN = sum(PN, na.rm = TRUE),
                  PN. = sum(PN., na.rm = TRUE)) %>%
        ungroup()

test$playoff[is.na(test$playoff)] <- FALSE

ffplayoff<- test %>%
        select(-c(team, season)) %>%
        ntbt(FFTrees, playoff ~ .)

plot(ffplayoff, 
     main = "20152016 NHL Playoffs (5v5)", decision.names = c("Is Not Playoff Team", "Is a Playoff Team"))