congress <- 115

get_cong_data <- function(congress, chamber) {
  res <- httr::GET(
    "https://api.propublica.org/congress/v1/{congress}/{chamber}" %PP%
      "/members.json",
    httr::add_headers("X-API-Key" = Sys.getenv("PROPUBLICA_KEY")))
  d <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  d <- d$results$members[[1]]
  d$chamber <- chamber
  d$congress <- as.character(congress)
  tibble::as_tibble(d)
}

h14 <- get_cong_data(114, "house")
s14 <- get_cong_data(114, "senate")
h15 <- get_cong_data(115, "house")
s15 <- get_cong_data(115, "senate")


cng <- dplyr::bind_rows(h14, h15, s14, s15)

save_RDS(cng, "data/congress-114-115.rds")


fn <- paste(cng$first_name, cng$last_name)
tn <- .s$name[!duplicated(.s$name)]
sum(tn %in% fn)
sum(fn %in% tn)

cng$sn2 <- .s$screen_name[!duplicated(.s$name)][match(tolower(fn), tolower(tn))]
select_data(cng, first_name, last_name, twitter_account, sn2)

library(dplyr)

cng %>%
  select(first_name, last_name, sn1 = twitter_account, sn2) %>%
  dplyr::mutate(
    sn = case_when(
      is.na(sn1) ~ sn2,
      TRUE ~ sn1
    )
  ) %>%
  pull(sn) %>%
  is.na() %>%
  which() ->stillmissing






fn <- paste(cng$first_name, cng$last_name)
tn <- .s$name

.s$sn2 <- cng$twitter_account[!duplicated(fn)][match(tolower(tn), tolower(fn))]

select_data(.s, name, screen_name, sn2)

library(dplyr)

c1 <- cng %>%
  select(id, sn2 = twitter_account) %>%
  mutate(sn2 = tolower(sn2))

c2 <- .s %>%
  mutate(sn1 = tolower(screen_name), sn2 = tolower(sn2)) %>%
  filter(sn1 == sn2 & !is.na(sn2)) %>%
  select(user_id, sn1 = screen_name, sn2) %>%
  unique()

cng %>%
  select(id, sn2 = twitter_account) %>%
  mutate(sn2 = tolower(sn2))


cng$user_id <- .s$user_id[match(tolower(cng$twitter_account), tolower(.s$screen_name))]

left_join(c1, c2) %>%
  select(id, sn2 = sn1, user_id2 = user_id) %>%
  left_join(select(cng, id, sn1 = twitter_account, user_id1 = user_id)) %>%
  unique() %>%
  select(id, sn1, sn2, user_id1, user_id2) %>%
  arrange(id) -> ids


rbind(filter(ids, !is.na(sn2)) %>%
  select(id, screen_name = sn2, user_id = user_id2) %>%
  unique(), filter(ids, !is.na(sn1)) %>%
  select(id, screen_name = sn1, user_id = user_id1) %>%
  unique()) %>%
  arrange(id) -> ids

ids

nac <- rtweet::lookup_users(tolower(ids$screen_name[is.na(ids$user_id)]))

ids$user_id[is.na(ids$user_id)] <- nac$user_id[match(tolower(ids$screen_name[is.na(ids$user_id)]), tolower(nac$screen_name))]


filter(ids, id == "T000475")

ids <- filter(ids, !duplicated(tolower(screen_name)))


ids %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  arrange(decr(n))
ids

.s %>%
  select(name, sn1 = screen_name, sn2) %>%
  unique() %>%
  dplyr::mutate(
    sn = case_when(
      is.na(sn1) ~ sn2,
      TRUE ~ sn1
    )
  ) %>%
  pull(sn) %>%
  is.na() %>%
  which() ->stillmissing


