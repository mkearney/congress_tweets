p <- rtweet::ts_plot(filter(d, created_at > "2017-06-01"), "weeks",
  trim = 2, color = "transparent") + geom_point() +
  geom_smooth(method = "loess")
p2 <- p + theme_mwk(base_size = 15) + labs(x = NULL, y= NULL,
  title = "Weekly tweet counts of midterm candidates for U.S. Congress",
  subtitle = "Aggregated number of Twitter statuses posted during 2018 midterm elections",
  caption = tfse::theme_mwk_caption_text())

ggsave("~/Dropbox/cand-tweet-ggplot.png", p2, width = 8,
  height = 6, units = "in")
