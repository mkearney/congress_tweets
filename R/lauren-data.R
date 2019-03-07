options(tbltools.print_tibble = FALSE)
library(kmw)

## candidates of interest
cands <- c(
  "McSally",
  "Sinema",
  "Hawley",
  "McCaskill",
  "Braun",
  "Donnelly"
)

## regex version of names
cands_re <- paste0(cands, collapse = "|")

## read in data
.d0 <- read_RDS(
  here("data/midterm-candidate-tweets2.rds")
)

## unique user IDs
uq <- !duplicated(.d0$user_id)
## look for matching screen names or names
kp <- .d0$user_id[uq][which(
  grepl(cands_re, .d0$name[uq], ignore.case = TRUE) |
    grepl(cands_re, .d0$screen_name[uq], ignore.case = TRUE)
)]

## filter data
.d0 <- filter_data(.d0, user_id %in% kp)

## read rest of data sets
.d1 <- read_RDS(
  here("data/midterm-candidate-tweets-2018-10-15.rds")
)
.d2 <- read_RDS(
  here("data/midterm-candidate-tweets-2018-10-19.rds")
)
.d3 <- read_RDS(
  here("data/midterm-candidate-tweets-2019-01-22.rds")
)

## filter those data ets
.d1 <- filter_data(.d1, user_id %in% kp)
.d2 <- filter_data(.d2, user_id %in% kp)
.d3 <- filter_data(.d3, user_id %in% kp)

## bind rows
.d <- bind_rows_data(.d0, .d1, .d2, .d3, fill = FALSE)

## variable class information
cols <- readr::cols(
  user_id = readr::col_character(),
  govtrack = readr::col_character(),
  title = readr::col_character(),
  short_title = readr::col_character(),
  first_name = readr::col_character(),
  middle_name = readr::col_character(),
  last_name = readr::col_character(),
  suffix = readr::col_character(),
  date_of_birth = readr::col_date(format = ""),
  gender = readr::col_character(),
  party = readr::col_character(),
  state = readr::col_character()
)
## read top-level congress data data
cng_toplevel <- readr::read_csv("data/congress-toplevel.csv", col_types = cols)
cng_toplevel$last_name

m1 <- cng_toplevel %>%
  as_tbl_data() %>%
  filter_data(grepl(cands_re, last_name, ignore.case = TRUE),
    state %in% c("IN", "MO", "AZ")) %>%
  select_data(-party)

p <- readr::read_csv("data/candidates_2018_0921.csv")
grep("Sinema", p$name, ignore.case = TRUE, value = TRUE)
p$name
m2 <- p %>%
  as_tbl_data() %>%
  mutate_data(
    first_name = regmatches_first(clean_name, "^\\S+", drop = TRUE),
    last_name = regmatches_first(clean_name, "\\S+$", drop = TRUE)
  ) %>%
  filter_data(grepl(cands_re, last_name, ignore.case = TRUE),
    state %in% c("AZ", "IN", "MO"))

m <- full_join_data(m1, m2) %>%
  select_data(-middle_name, -suffix, -crp_id, -google_entity_id)

names(m)[names(m) == c("url")] <- "cand_url"
names(m)[names(m) == c("name")] <- "cand_name"

.d <- left_join_data(.d, m)

.d <- .d %>%
  filter_data(created_at < as.POSIXct("2018-11-07", tz = "UTC"))

.d %>%
  arrange_data(screen_name) %>%
  rtweet::save_as_csv(here("data", "az-in-mo.csv"))


nrow(.d)

.d <- .d %>%
  filter_data(!is_retweet)


uq <- unique(.d$user_id)

n <- 100
rows <- unlist(lap(uq, ~ sample(which(.d$user_id == .x), n)))
slice_data(.d, rows) %>%
  arrange_data(screen_name) %>%
  rtweet::save_as_csv(here("data", "az-in-mo-n100.csv"))

n <- 200
rows <- unlist(lap(uq, ~ sample(which(.d$user_id == .x), n)))
slice_data(.d, rows) %>%
  arrange_data(screen_name) %>%
  rtweet::save_as_csv(here("data", "az-in-mo-n200.csv"))

