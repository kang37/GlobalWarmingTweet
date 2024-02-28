# function: read raw *.csv data
# argument: 
# file.dir: direction of the files
# file.name: file name of the *.csv data
ReadTwRaw <- function(file.dir, file.name) {
  read.csv(paste0(file.dir, "/", file.name)) %>% 
    tibble() %>% 
    # parse created_at of tweets to get year and month info
    mutate(date = as.Date(created_at)) %>% 
    select(id, date, text) %>% 
    mutate(
      year = year(date), month = month(date), day = day(date), .after = date
    ) %>% 
    return()
}

# function: get subset of peak period with certian key words 
# x: data.frame, tw.peak.gw ro tw.peak.cc
TocPeak <- function(x) {
  as.character(x) %>% 
    split(
      ., 
      rep(1:6, each = ceiling(length(.)/6), length.out = length(.))
    ) %>% 
    mclapply(
      FUN = tokens, 
      remove_punct = FALSE, 
      remove_numbers = FALSE,
      remove_url = FALSE,
      mc.cores = 4
    ) %>% 
    do.call(c, .) %>%  
    # only keep hirakana and katakana
    tokens_select(
      pattern = "^[ー一-龠]+$", valuetype = "regex", padding = TRUE
    ) %>% 
    tokens_remove(pattern = c("ー", "")) %>% 
    tokens_compound(pattern = dict, concatenator = "") %>% 
    return()
}

# function: get high-freqency word plot
# argument: 
# x: dfm object, dfm.peak.gw or dfm.peak.cc
# keyword: "GW" or "CC"
PltHighFreqWord <- function(x, keyword) {
  dt.wordfreq.peak <- x %>% 
    dfm_remove(pattern = c("温暖化", "地球温暖化")) %>% 
    dfm_weight(scheme = "prop") %>% 
    textstat_frequency(n = 15, groups = peak_grp)
  
  png(paste0("ProcData/High_freq_word_by_peak_", keyword, ".png"), 
      width = 5000, height = 5000, res = 300)
  plt.wordfreq.peak <- 
    ggplot(data = dt.wordfreq.peak, 
           aes(x = nrow(dt.wordfreq.peak):1, y = frequency)) +
    geom_point() +
    facet_wrap(~ group, scales = "free") +
    coord_flip() +
    scale_x_continuous(breaks = nrow(dt.wordfreq.peak):1,
                       labels = dt.wordfreq.peak$feature) +
    labs(x = NULL, y = "Relative frequency")
  print(plt.wordfreq.peak)
  dev.off()
}

# function: plot two varables, one as column and the other as line 
# argument: 
# x: data.frame of target period
# file: prefix of file name
# var_col: varaibale shown as column
# var_line: varaible shown as red line
# times: times for second variable (the red line one) and scale of second y-axis
# name_y1: name of first y-axis
# name_y2: name of second y-axis
PlotColLine <- function(x, file, var_col, var_line, times, name_y1, name_y2) {
  png(paste0("ProcData/", file, "_", var_col, "_", var_line, ".png"), 
      width = 1000, height = 800, res = 300)
  plt <- ggplot(x) + 
    geom_col(
      aes(x = date, y = get(var_col)), fill = "grey") + 
    labs(x = "", y = name_y1) +
    theme(axis.ticks.x = element_blank(), 
          panel.grid.major = element_blank()) + 
    geom_line(
      aes(date, get(var_line) * times, group = 1), 
      col = "red", alpha = 0.7) + 
    scale_y_continuous(
      sec.axis = sec_axis(~./times, name = name_y2))
  print(plt)
  dev.off()
  return(plt)
}
