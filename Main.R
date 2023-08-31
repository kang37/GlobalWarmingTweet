# Statement ----
# Analyze the Japanese tweets of "global warming" and "climate change": to identify the tweets of peak events. 

# Preparation ----
# Transform Twitter data from *.json to *.jsonl, then to *.csv using twarc2 in terminal.
# An example of the first step (change directory to the folder first): 
# twarc2 flatten jst201201.json jst201201.jsonl
# And an example of the second step, only extracting the data required for this study: 
# twarc2 csv --no-inline-referenced-tweets --output-columns "id,created_at,author_id,author.username,author.name,author.description,author.public_metrics.followers_count,text,retweeted_user_id,public_metrics.retweet_count" jst201201.jsonl 201201.csv
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

# High centrality user's participation in events
# Bug: Take betweenness centrality as the example for now. 
png("data_proc/plot_high_cen_user_event.png", 
    width = 2000, height = 8000, res = 300)
lapply(
  names(top_between), 
  function(x) {
    mutate(top_between[[x]], event = x, .before = 1) %>% 
      select(event, author_username)
  }
) %>% 
  bind_rows() %>% 
  ggplot() + 
  geom_tile(aes(event, author_username), col = "white")
dev.off()

# Distribution of the event number that higher centrality users has impacts on. 
tar_load(author_attr)

# Distribution of participate number of high-degree-centrality users. 
# Bug: Here I use author_username to group the data because that we can understand the tile plot easier, but it should be done with userid rather than author_username because a userid may have multiple author_username. 
degree_user_event_num <- lapply(
  names(top_degree), 
  function(x) {
    mutate(top_degree[[x]], event = x, .before = 1) %>% 
      select(event, userid)
  }
) %>% 
  bind_rows() %>% 
  group_by(userid) %>% 
  summarise(n_event = n(), .groups = "drop") %>% 
  arrange(-n_event) %>% 
  left_join(
    author_attr %>% 
      group_by(author_id) %>% 
      summarise(
        author_username = 
          paste(unique(author_username), collapse = "----"), 
        author_name = 
          paste(unique(author_name), collapse = "----"), 
        max_followers_count = max(followers_count), 
        author_description = 
          paste(unique(author_description), collapse = "----")
      ), 
    by = c("userid" = "author_id")
  )
degree_user_event_num %>% 
  group_by(n_event) %>% 
  summarise(user_number = n(), .groups = "drop") %>% 
  ggplot(aes(n_event, user_number)) + 
  geom_col() + 
  geom_text(aes(label = user_number), vjust = -1, size = 3) + 
  lims(y = c(0, 255)) + 
  scale_x_continuous(breaks = seq(0, 15, 1))
write.csv(degree_user_event_num, "data_proc/degree_user_event_num.csv")

# Distribution of participate number of high-between-centrality users. 
# Bug: Here I use author_username to group the data because that we can understand the tile plot easier, but it should be done with userid rather than author_username because a userid may have multiple author_username. 
between_user_event_num <- lapply(
  names(top_between), 
  function(x) {
    mutate(top_between[[x]], event = x, .before = 1) %>% 
      select(event, userid)
  }
) %>% 
  bind_rows() %>% 
  group_by(userid) %>% 
  summarise(n_event = n(), .groups = "drop") %>% 
  arrange(-n_event) %>% 
  left_join(
    author_attr %>% 
      group_by(author_id) %>% 
      summarise(
        author_username = 
          paste(unique(author_username), collapse = "----"), 
        author_name = 
          paste(unique(author_name), collapse = "----"), 
        max_followers_count = max(followers_count), 
        author_description = 
          paste(unique(author_description), collapse = "----")
      ), 
    by = c("userid" = "author_id")
  )
between_user_event_num %>% 
  group_by(n_event) %>% 
  summarise(user_number = n(), .groups = "drop") %>% 
  ggplot(aes(n_event, user_number)) + 
  geom_col() + 
  geom_text(aes(label = user_number), vjust = -1, size = 3) + 
  lims(y = c(0, 255)) + 
  scale_x_continuous(breaks = seq(0, 15, 1))
write.csv(between_user_event_num, "data_proc/between_user_event_num.csv")

# User-events-distribution by tags. 
rbind(
  read.csv("data_raw/manual_data/degree_user_event_num_tag.csv") %>% 
    mutate(centrality = "Between cen") %>% 
    select(centrality, tag, n_event), 
  read.csv("data_raw/manual_data/between_user_event_num_tag.csv") %>% 
    mutate(centrality = "Degree cen") %>% 
    select(centrality, tag, n_event)
) %>% 
  # Keep user-events larger than 1. 
  filter(n_event > 1) %>% 
  group_by(centrality, tag) %>% 
  summarise(n_event = sum(n_event), .groups = "drop") %>% 
  group_by(centrality) %>% 
  mutate(
    tot_event_num = sum(n_event), 
    prop_event = n_event / tot_event_num * 100
  ) %>% 
  ggplot(aes(x = centrality, y = prop_event, fill = tag)) + 
  geom_col(position = "stack") + 
  geom_text(
    aes(label = round(prop_event)), position = position_stack(vjust = .5), size = 3
  ) + 
  coord_flip() + 
  guides(fill = guide_legend(title = "Tag"))

