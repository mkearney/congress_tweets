cand_mentions <- readRDS("~/Desktop/cand_mentions.rds")

rtweet::ts_plot(cand_mentions, "hours", trim = 4) +
  theme_mwk() +
  labs(title = "U.S. Congressional candidate mention frequency on Twitter",
    subtitle = "Hourly number of non-retweet mentions (N = 593,094) of candidates for U.S. Congress",
    caption = theme_mwk_caption_text(), x = NULL, y = NULL) +
  ggsave("~/Dropbox/cand-men-freq.png", width = 6, height = 4, units = "in")
