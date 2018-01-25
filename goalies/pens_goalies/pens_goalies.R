library(tidyverse)

theme_set(theme_bw())

file <- list.files("pens_goalies")

df <- read_csv(str_c("pens_goalies/", file))

colnames(df) <- tolower(colnames(df))

df <- df %>% 
  rename(sv_per = `sv%`,
         xsv_per = `xsv%`,
         dsv_per = `dsv%`)

df %>% 
  filter(!(player == "ANTTI.NIEMI")) %>% 
  ggplot(aes(sv_per, xsv_per, label = player)) +
  geom_label()

df %>% 
  ggplot(aes(player, dsv_per, alpha = sa)) +
  geom_hline(yintercept = 0) +
  geom_col(color = "black") +
  scale_alpha_continuous("Shots Against") +
  labs(y = "Save % - Expected Sv%",
       x = "",
       caption = "@Null_HHockey, Data from corsica.hockey")

ggsave("pens_goalies/pens_goalies.png")
