# Statement ----
# Analyze the Japanese tweets of "global warming" and "climate change": to identify the tweets of peak events. 

# Preparation ----
pacman::p_load(targets, dplyr, lubridate, ggplot2, tidygraph, ggraph)
tar_make()
tar_load(tw_high_90)
tar_load(tw_high_85)
tar_load(graph_cen)
tar_load(net_plot)

# Analysis ----
## Identify peak event ----
tw_high_90 %>% 
  mutate(
    day = day(date),
    date_dummy = as.Date(paste("9999", month, day, sep = "-"))
  ) %>% 
  ggplot() + 
  geom_tile(aes(date_dummy, 1), fill = "darkred") + 
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    panel.grid = element_blank()
  ) +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Date", y = "") + 
  facet_wrap(.~ year, ncol = 1, strip.position = "right")

tw_high_90 %>% 
  select(year, grp) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(peak_num = n()) %>% 
  ungroup()

tw_high_85 %>% 
  mutate(
    day = day(date),
    date_dummy = as.Date(paste("9999", month, day, sep = "-"))
  ) %>% 
  ggplot() + 
  geom_tile(aes(date_dummy, 1), fill = "darkred") + 
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(), 
    panel.grid = element_blank()
  ) +
  scale_x_date(date_labels = "%b") + 
  labs(x = "Date", y = "") + 
  facet_wrap(.~ year, ncol = 1, strip.position = "right")

tw_high_85 %>% 
  select(year, grp) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarise(peak_num = n()) %>% 
  ungroup()

## Event network ----
# Top users with high centrality values
lapply(
  graph_cen, 
  function(x) filter(x, mvp_grp != "no_mvp")
)
# Network plots
net_plot
