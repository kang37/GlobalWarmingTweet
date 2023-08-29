# Statement ----
# Analyze the Japanese tweets of "global warming" and "climate change": to identify the tweets of peak events. 

# Preparation ----
# Transform Twitter data from *.json to *.jsonl, then to *.csv using twarc2 in terminal.
# An example of the first step (change directory to the folder first): 
# twarc2 flatten jst201201.json jst201201.jsonl
# And an example of the second step, only extracting the data required for this study: 
# twarc2 csv --no-inline-referenced-tweets --output-columns "id,created_at,author_id,author.username,author.description,author.public_metrics.followers_count,text,retweeted_user_id,public_metrics.retweet_count" jst201201.jsonl 201201.csv
# Bug: The second step caused some errors, so I ran the command lines in terminal panel of RStudio. 
pacman::p_load(
  targets, dplyr, lubridate, ggplot2, tidygraph, ggraph, openxlsx, quanteda
)
tar_make()

# Analysis ----
## Identify peak event ----
tar_load(tw_high_90)
tar_load(tw_high_85)

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
tar_load(net_plot_cen)
tar_load(net_plot_comm)
# Network plots with centrality information. 
net_plot_cen
# Network plots with community information. 
net_plot_comm

## Data export ----
# Start and end date of each event.
tar_load(event_90)
write.csv(event_90, "data_proc/event_date.csv")

# Raw data of each event. 
tar_load(csv_raw)
lapply(
  names(csv_raw), 
  function(x) write.csv(csv_raw[[x]], paste0("data_proc/event_raw/", x, ".csv"))
)

# Users with high degree centrality. 
tar_load(top_degree)
write.xlsx(top_degree, "data_proc/top_degree.xlsx")

# Users with high between centrality. 
tar_load(top_between)
write.xlsx(top_between, "data_proc/top_between.xlsx")

# Top frequency terms
tar_load(df_matrix)
top_term <- 
  lapply(
    df_matrix, 
    function(x) {
      topfeatures(x, n = 30) %>% 
        as.data.frame() %>% 
        rename_with(~ "freq") %>% 
        mutate(term = rownames(.), .before = 1) %>% 
        tibble()
    }
  )
write.xlsx(top_term, "data_proc/top_frequency_term.xlsx")
