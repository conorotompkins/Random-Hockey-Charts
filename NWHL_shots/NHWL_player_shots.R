library(tidyverse)

#http://www.nwhl.zone/stats/league_instance/46941?order_by=hksgm&order_dir=desc&stat_tab=ice_hockey_skater&subseason=327125

page_1 <- read.delim("clipboard")
page_2 <- read.delim("clipboard")
page_3 <- read.delim("clipboard")
my_files <- list(page_1 = page_1,
                 page_2 = page_2,
                 page_3 = page_3)

my_data <- bind_rows(my_files)
