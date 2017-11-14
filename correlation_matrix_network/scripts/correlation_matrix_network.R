library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)

#https://drsimonj.svbtle.com/how-to-create-correlation-network-plots-with-corrr-and-ggraph

set.seed(1234)

data <- read_csv("correlation_matrix_network/data/team_stats_0708_1617.csv")

colnames(data) <- tolower(colnames(data))
colnames(data) <- str_replace(colnames(data), "%", "_percent")
colnames(data) <- str_replace(colnames(data), "/60", "_per60")

data %>% 
  rename(c_diff = `c+/-`,
         g_diff = `g+/-`,
         xg_diff = `xg+/-`,
         pen_diff = `p+/-`,
         pen_taken = pent,
         pen_drawn = pend) -> data

data %>% 
  select(-c(gp, toi)) %>% 
  select_if(is.numeric) -> data

tidy_cors <- data %>% 
  correlate() %>% 
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object
graph_cors <- tidy_cors %>% 
  filter(abs(r) >= .4) %>% 
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn("R", limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "black", fill = "white", size = 5, shape = 1) +
  geom_node_label(aes(label = name), repel = TRUE) +
  scale_edge_width_continuous(range = c(.01, 2)) +
  scale_edge_alpha_continuous(range = c(.01, 1)) +
  theme_graph() +
  labs(title = "Correlations between Corsica variables at the team-season level",
       subtitle = "2007-2008 to 2016-2017, Absolute R >= .4")

ggsave("correlation_matrix_network/images/correlation_matrix_network_graph.png", width = 12, height = 12)