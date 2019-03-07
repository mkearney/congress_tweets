#options(tbltools.print_tibble = FALSE)
library(kmw)

## read in data
.d0 <- read_RDS(
  here("data/midterm-candidate-tweets2.rds")
)
.d1 <- read_RDS(
  here("data/midterm-candidate-tweets-2018-10-15.rds")
)
.d2 <- read_RDS(
  here("data/midterm-candidate-tweets-2018-10-19.rds")
)
.d3 <- read_RDS(
  here("data/midterm-candidate-tweets-2019-01-22.rds")
)
## bind rows
.d <- bind_rows_data(.d0, .d1, .d2, .d3, fill = FALSE)
rm(.d0, .d1, .d2, .d3)

## filter out everyhing after the election
.d <- .d %>%
  filter_data(created_at < as.POSIXct("2018-11-08 00:00:00", tz = "UTC"))

## remove duplicates
.d <- dplyr::filter(.d, !duplicated(status_id))

## only tweets from last year
.d <- .d %>%
  dplyr::filter(created_at >= as.POSIXct("2017-11-01 00:00:00", tz = "UTC"))

## plot time series
.d %>%
  rtweet::ts_plot("weeks", trim = 1, size = 1) +
  theme_mwk(base_size = 18) -> p

p + ggplot2::labs(
  title = "Weekly tweet counts for all candidates in the 2018 midterms",
  x = NULL, y = NULL) +
  ggplot2::ylim(0, 25000)

.d

p <- readr::read_csv("data/candidates_2018_0921.csv")
p <- p %>%
  as_tbl_data() %>%
  mutate_data(
    first_name = regmatches_first(clean_name, "^\\S+", drop = TRUE),
    last_name = regmatches_first(
      sub(" (II|III|Jr\\.)$", "", clean_name), "\\S+$", drop = TRUE)
  )

p$last_name
uq_last <- unique(.d$name)
uq_last <- regmatches_first(sub(" (II|III|Jr\\.)$", "", uq_last), "\\S+$", drop = TRUE)


pd <- data.frame(
  name = unique(.d$name),
  party = p$party[match(tolower(uq_last), tolower(p$last_name))],
  stringsAsFactors = FALSE
)

.d <- left_join_data(.d, pd)

## plot time series
p <- .d %>%
  filter_data(!is.na(party)) %>%
  mutate_data(party = ifelse(party %in% c("DEM", "REP"), party, "3RD")) %>%
  filter_data(created_at >= as.POSIXct("2017-01-01 00:00:00", tz = "UTC")) %>%
  dplyr::group_by(party) %>%
  rtweet::ts_plot("months") +
  theme_mwk()

library(ggplot2)

p +
  geom_point() +
  scale_color_manual(
    values = c(REP = "#dd2222", DEM = "#2244ee", `3RD` = "#cccc00")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) formatC(x, digits = 10, big.mark = ",")
  ) +
  scale_x_datetime(
    date_breaks = "3 months",
    labels = function(x) format(x, "%b %Y")
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Monthly tweet counts of candidates for U.S. Congress by party",
    subtitle = "Based on total number of major/3rd party-tweets posted from Jan. 1, 2017 to Nov. 6, 2018",
    caption = theme_mwk_caption_text()
  ) +
  ggsave("~/Dropbox/ussm2.png", width = 7, height = 5.5, units = "in")
