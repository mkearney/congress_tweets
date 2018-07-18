
tml$sent_afinn <- syuzhet::get_sentiment(tml$text, method = "afinn")

## post 2016 plot
post <- tml %>%
  left_join(select(cng_toplevel, user_id, govtrack, gender, party), by = "user_id") %>%
  filter(party %in% c("R", "D"), !is_retweet, created_at > "2016-11-01") %>%
  mutate(sent = scale(sent_afinn)[, 1]) %>%
  group_by(govtrack) %>%
  summarise(n = n(), sent = mean(sent, trim = .01),
    party = party[1], gender = gender[1]) %>%
  mutate(x = (gender == "M") + (party == "R") * 2,
    x = factor(x, labels = c("W (D)", "M (D)", "W (R)", "M (R)"))) %>%
  ggplot(aes(x = x, y = sent, fill = party)) +
  geom_hline(yintercept = 0, colour = "#00000055", size = .5) +
  geom_boxplot(alpha = .3, colour = "#000000cc", size = .15, outlier.colour = "transparent") +
  geom_jitter(size = 3, alpha = .3, shape = 21, width  = .33) +
  tfse::theme_mwk() +
  scale_fill_manual(values = c(R = "#dd3333", D = "#2255ee")) +
  labs(x = "Gender (Party)", y = "Mean Sentiment Score", title = "After November 2016") +
    #title = "POST Nov 2016 - Sentiment of Members of Congress Tweets by Party and Gender",
    #subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account") +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = rel(1.30), hjust = .5),
    plot.subtitle = element_text(size = rel(1.05)),
    axis.text.x = element_text(size = rel(1.10)),
    axis.text.y = element_text(size = rel(1.20)),
    plot.margin = margin(1, 0, 0, 0, unit = "lines")) +
  ylim(-.7, .8) +
  coord_flip()

## pre nov 2016 plot
pre <- tml %>%
  left_join(select(cng_toplevel, user_id, govtrack, gender, party), by = "user_id") %>%
  filter(party %in% c("R", "D"), !is_retweet, created_at < "2016-11-01") %>%
  mutate(sent = scale(sent_afinn)[, 1]) %>%
  group_by(govtrack) %>%
  summarise(n = n(), sent = mean(sent, trim = .01),
    party = party[1], gender = gender[1]) %>%
  mutate(x = (gender == "M") + (party == "R") * 2,
    x = factor(x, labels = c("W (D)", "M (D)", "W (R)", "M (R)"))) %>%
  ggplot(aes(x = x, y = sent, fill = party)) +
  geom_hline(yintercept = 0, colour = "#00000055", size = .5) +
  geom_boxplot(alpha = .3, colour = "#000000cc", size = .15, outlier.colour = "transparent") +
  geom_jitter(size = 3, alpha = .3, shape = 21, width  = .33) +
  tfse::theme_mwk() +
  scale_fill_manual(values = c(R = "#dd3333", D = "#2255ee")) +
  labs(x = "Gender (Party)", y = "Mean Sentiment Score", title = "Before November 2016") +
#    title = "PRE Nov 2016 - Sentiment of Members of Congress Tweets by Party and Gender",
#    subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account") +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = rel(1.30), hjust = .5),
    plot.subtitle = element_text(size = rel(1.05)),
    axis.text.x = element_text(size = rel(1.10)),
    axis.text.y = element_text(size = rel(1.20)),
    plot.margin = margin(1, 0, 0, 0, unit = "lines")) +
  ylim(-.7, .8) +
  coord_flip()

## use patchwork to combine plots
library(patchwork)

## combine and save
pre + post + plot_layout(ncol = 1) +
  plot_annotation(title = "Sentiment of Tweets - Members of Congress - Before & After Nov 2016",
    subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account",
    theme = theme(plot.title = element_text(face = "bold", size = rel(1.70)),
      plot.subtitle = element_text(size = rel(1.1)),
      plot.caption = element_text(hjust = 0)),
    caption = "Source: Statuses (N = 1.3 million) collected from REST API via rtweet; sentiment scores from \"afinn\" dictionary via syuzhet") +
  ggsave("~/Desktop/sent-prepost.png")

## COMPARE WITH PYTHON VADER SCORES

## vader scores
vd <- readr::read_csv("~/Desktop/test.txt", col_names = c("status_id", "score"),
  col_types = cols(status_id = col_character(), score = col_double()))

vd <- left_join(select(tml, status_id, sent_afinn), vd)

vd <- select(vd, status_id, afinn = sent_afinn, vader = score)
vd <- vd %>% mutate(afinn = as.numeric(afinn))
vd$vader[vd$status_id == "91528896507424768"] <- .4019

vd %>%
  sample_n(15000) %>%
  mutate(afinn = scale(afinn)[, 1],
    vader = scale(vader)[, 1]) %>%
  ggplot(aes(x = afinn, y = vader)) +
  geom_jitter(alpha = .25) +
  geom_smooth() +
  #coord_cartesian(xlim = c(-2.25, 2.25), ylim = c(-2.25, 2.25)) +
  ggsave("~/Desktop/vadervsafinn.png")


tml <- left_join(tml, select(vd, status_id, vader))


pre_vader <- tml %>%
  left_join(select(cng_toplevel, user_id, govtrack, gender, party), by = "user_id") %>%
  filter(party %in% c("R", "D"), !is_retweet, created_at < "2016-11-01") %>%
  mutate(sent = scale(vader)[, 1]) %>%
  group_by(govtrack) %>%
  summarise(n = n(), sent = mean(sent, trim = .01),
    party = party[1], gender = gender[1]) %>%
  mutate(x = (gender == "M") + (party == "R") * 2,
    x = factor(x, labels = c("W (D)", "M (D)", "W (R)", "M (R)"))) %>%
  ggplot(aes(x = x, y = sent, fill = party)) +
  geom_hline(yintercept = 0, colour = "#00000055", size = .5) +
  geom_boxplot(alpha = .3, colour = "#000000cc", size = .15, outlier.colour = "transparent") +
  geom_jitter(size = 3, alpha = .3, shape = 21, width  = .33) +
  tfse::theme_mwk() +
  scale_fill_manual(values = c(R = "#dd3333", D = "#2255ee")) +
  labs(x = "Gender (Party)", y = "Mean Sentiment Score", title = "Before November 2016") +
  #    title = "PRE Nov 2016 - Sentiment of Members of Congress Tweets by Party and Gender",
  #    subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account") +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = rel(1.30), hjust = .5),
    plot.subtitle = element_text(size = rel(1.05)),
    axis.text.x = element_text(size = rel(1.10)),
    axis.text.y = element_text(size = rel(1.20)),
    plot.margin = margin(1, 0, 0, 0, unit = "lines")) +
  ylim(-.7, .8) +
  coord_flip()


post_vader <- tml %>%
  left_join(select(cng_toplevel, user_id, govtrack, gender, party), by = "user_id") %>%
  filter(party %in% c("R", "D"), !is_retweet, created_at > "2016-11-01") %>%
  mutate(sent = scale(vader)[, 1]) %>%
  group_by(govtrack) %>%
  summarise(n = n(), sent = mean(sent, trim = .01),
    party = party[1], gender = gender[1]) %>%
  mutate(x = (gender == "M") + (party == "R") * 2,
    x = factor(x, labels = c("W (D)", "M (D)", "W (R)", "M (R)"))) %>%
  ggplot(aes(x = x, y = sent, fill = party)) +
  geom_hline(yintercept = 0, colour = "#00000055", size = .5) +
  geom_boxplot(alpha = .3, colour = "#000000cc", size = .15, outlier.colour = "transparent") +
  geom_jitter(size = 3, alpha = .3, shape = 21, width  = .33) +
  tfse::theme_mwk() +
  scale_fill_manual(values = c(R = "#dd3333", D = "#2255ee")) +
  labs(x = "Gender (Party)", y = "Mean Sentiment Score", title = "After November 2016") +
  #title = "POST Nov 2016 - Sentiment of Members of Congress Tweets by Party and Gender",
  #subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account") +
  theme(legend.position = "none",
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(size = rel(1.30), hjust = .5),
    plot.subtitle = element_text(size = rel(1.05)),
    axis.text.x = element_text(size = rel(1.10)),
    axis.text.y = element_text(size = rel(1.20)),
    plot.margin = margin(1, 0, 0, 0, unit = "lines")) +
  ylim(-.7, .8) +
  coord_flip()

pre_vader


pre_vader + post_vader + plot_layout(ncol = 1) +
  plot_annotation(title = "Sentiment of Tweets - Members of Congress - Before & After Nov 2016",
    subtitle = "Mean [normalized] sentiment scores estimated from up to most recent 3,200 tweets per account",
    theme = theme(plot.title = element_text(face = "bold", size = rel(1.70)),
      plot.subtitle = element_text(size = rel(1.1)),
      plot.caption = element_text(hjust = 0)),
    caption = "Source: Statuses (N = 1.3 million) collected from REST API via rtweet; sentiment scores from \"VADER\" dictionary via vaderSentiment") +
  ggsave("~/Desktop/sent-prepost-vader.png")

## correlation coefficient
with(vd, cor(vader, afinn))

## compare distributions
vd %>%
  mutate(afinn = scale(afinn)[, 1],
    vader = scale(vader)[, 1]) %>%
  tidyr::gather(dict, score, -status_id) %>%
  ggplot(aes(x = score, fill = dict)) +
  stat_density(alpha = .5, adjust = 3.25, position = "identity") +
  coord_cartesian(xlim = c(-4, 4)) +
  ggsave("~/Desktop/densities.png")
