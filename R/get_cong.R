get_cong_data <- function(congress, chamber) {
  congress <- as.character(congress)
  url <- glue::glue(
    "https://api.propublica.org/congress/v1/{congress}/{chamber}/members.json")
  res <- httr::GET(
    url, httr::add_headers("X-API-Key" = Sys.getenv("PROPUBLICA_KEY")))
  d <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  d <- d$results$members[[1]]
  d$chamber <- chamber
  d$congress <- congress
  tibble::as_tibble(d)
}

h14 <- get_cong_data(114, "house")
s14 <- get_cong_data(114, "senate")
h15 <- get_cong_data(115, "house")
s15 <- get_cong_data(115, "senate")

h16 <- get_cong_data(116, "house")
s16 <- get_cong_data(116, "senate")



hs <- dplyr::bind_rows(h16, s16)
hs

tfse::yin(names(dw), names(hs))
hs$bi
library(dplyr)
dplyr::left_join(dw, dplyr::select(hs, -congress, -chamber) %>% dplyr::rename(bioguide_id = id)) ->
  d

readr::write_csv(d, "~/Dropbox/hscng.csv", na = "")
d$fec_candidate_id
nrow(dw)
nrow(hs)

match(dw$bioguide_id, hs$id)

tfse::nin(names(h16), names(s16))
tfse::nin(names(s16), names(h16))
hs
dw <- readr::read_csv("https://voteview.com/static/data/out/members/HS116_members.csv")
dw
with(dw, table(party_code, state_abbrev))

## Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin, and Luke Sonnet (2019). Voteview: Congressional Roll-Call Votes Database. https://voteview.com/

library(dplyr)

cng <- dplyr::bind_rows(h14, h15, s14, s15)


library(tfse)
save_RDS(cng, "data/congress-114-115.rds")

.s <- fst::read_fst("data/tweets-2017-2018-flat-small.fst")

fn <- paste(cng$first_name, cng$last_name)
tn <- .s$name[!duplicated(.s$name)]
sum(tn %in% fn)
sum(fn %in% tn)

cng$sn2 <- .s$screen_name[!duplicated(.s$name)][match(tolower(fn), tolower(tn))]


# cng %>%
#   select(first_name, last_name, sn1 = twitter_account, sn2) %>%
#   dplyr::mutate(
#     sn = case_when(
#       is.na(sn1) ~ sn2,
#       TRUE ~ sn1
#     )
#   ) %>%
#   pull(sn) %>%
#   is.na() %>%
#   which() -> stillmissing
#

fn <- paste(cng$first_name, cng$last_name)
tn <- .s$name

.s$sn2 <- cng$twitter_account[!duplicated(fn)][match(tolower(tn), tolower(fn))]


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

nac <- rtweet::lookup_users(tolower(ids$screen_name[is.na(ids$user_id)]))

ids$user_id[is.na(ids$user_id)] <- nac$user_id[match(tolower(ids$screen_name[is.na(ids$user_id)]), tolower(nac$screen_name))]

ids <- filter(ids, !duplicated(tolower(screen_name)))


cng %>%
  arrange(desc(congress)) %>%
  select(id, screen_name = twitter_account,
    first = first_name,
    middle = middle_name,
    last = last_name,
    gender, party, dw_nominate, state, district,
    at_large, geoid, chamber) %>%
  mutate(screen_name = tolower(screen_name),
    middle = ifelse(is.na(middle), "", middle),
    full = paste(first, last),
    full_m = paste(first, middle, last)) %>%
  filter(!duplicated(id)) -> g

u <- rtweet::lookup_users(tfse::na_omit(g$screen_name), token = rtweet::bearer_token())
g$user_id <- u$user_id[match(tolower(g$screen_name), tolower(u$screen_name))]

g$at_large <- NULL
g$geoid <- NULL

g <- select(g, id, user_id, screen_name:full_m)

tfse::save_RDS(g, "data/congress-ids-114-115.rds")



tfse::save_RDS(g, "data/congress-ids-114-115.rds")


library(tfse)
library(dplyr)

g <- tfse::read_RDS("data/congress-ids-114-115.rds")
.s <-  tibble::as_tibble(fst::read_fst("data/tweets-2017-2018-flat-small.fst"))
#.s <- tfse::read_RDS("data/tweets-2017-2018-flat-small.rds")

cng <- tfse::read_RDS("data/congress-114-115.rds")


sn2 <- .s$screen_name[match(tolower(g$full), tolower(.s$name))]
sn2 <- tolower(sn2)

g$screen_name2 <- sn2

g <- mutate(g,
  screen_name2 = ifelse(screen_name == screen_name2, NA_character_, screen_name2)
)

f <- function(x) {
  x$description <- gsub("(vs|unsea\\S+|against|challeng\\S+|oppos\\S+).{5,20}\\s",
    "", x$description, ignore.case = TRUE)
  nms <- tfse::regmatches_(x$description, "[A-Z][a-z]\\w+ [A-Z][a-z]\\w+", drop = TRUE)
  i <- which(paste(g$first, g$last) %in% nms)
  if (length(i) == 0) {
    x <- tibble::tibble(user_id = x$user_id,
      screen_name = tolower(x$screen_name),
      other1 = NA_character_,
      other2 = NA_character_,
      id = NA_character_)
    return(x)
  }
  x <- tibble::tibble(user_id = x$user_id,
    screen_name = tolower(x$screen_name),
    other1 = g$screen_name[i],
    other2 = g$screen_name2[i],
    id = g$id[i])
  unique(x)
}
ud <- select(filter(.s, !duplicated(user_id)), user_id, screen_name, description)

udo <- dapr::lap(ud$user_id, ~ f(dplyr::filter(ud, user_id == .x)))
udo <- dplyr::bind_rows(udo)


idlong <- tidyr::gather(select(udo, -user_id), var, screen_name, -id) %>%
  select(-var) %>%
  filter(!is.na(screen_name), !is.na(id)) %>%
  unique() %>%
  rbind(select(g, id, screen_name), .) %>%
  unique() %>%
  filter(!is.na(screen_name), !duplicated(screen_name)) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(n=n()) %>%
  arrange(desc(n), id) %>%
  ungroup()

u <- rtweet::lookup_users(idlong$screen_name)

idlong$user_id <- u$user_id[match(idlong$screen_name, tolower(u$screen_name))]

did <- .s$user_id[match(idlong$screen_name, tolower(.s$screen_name))]

idlong$user_id[is.na(idlong$user_id)] <- did[is.na(idlong$user_id)]


idlong <- filter(idlong, !is.na(user_id))

idwide <- idlong %>%
  group_by(id) %>%
  summarise(screen_name = paste(screen_name, collapse = " "),
    user_id = paste(user_id, collapse = " "),
    n = max(n)) %>%
  arrange(desc(n)) %>%
  print(n = 25)


dud <- unique(select(.s, user_id))
ids <- idlong$id[match(dud$user_id, idlong$user_id)]

dud$id <- ids

d <- left_join(.s, dud)
dd <- left_join(d, filter(select(cng, id, party, date_of_birth, gender, state,
  chamber), !duplicated(id)))


filter(dd, is.na(party)) %>%
  pull(screen_name) %>%
  unique() -> unna

unna[1]

cng$first_name[grep("Peterson", cng$last_name)]

cng <- filter(cng, !duplicated(id))

cands <- readr::read_csv("data/candidates_2018_0921.csv")


g$crp_id <- cng$crp_id[match(g$id, cng$id)]
cands$id <- cng$id[match(cands$crp_id, g$crp_id)]

nacands <- cands#filter(cands, is.na(id))
nad <- filter(dd, is.na(id)) %>%
  select(user_id, screen_name, name, description) %>%
  filter(!duplicated(user_id))


nad$crp_id <- nacands$crp_id[match(tolower(nad$name), tolower(nacands$clean_name))]
lns <- sub(",.*", "", tolower(nacands$name))



nad$crp_id2 <- dapr::vap_chr(tokenizers::tokenize_words(nad$name), function(.n) {
  i <- which(.n %in% lns)
  if (length(i) == 0) return(NA_character_)
  nacands$crp_id[i[1]]
})


cng <- filter(cng, !duplicated(id))

uud <- select(d, screen_name, user_id) %>% distinct()


cng$user_id <- uud$user_id[match(tolower(cng$twitter_account), tolower(uud$screen_name))]

nnnnn <- rtweet::lookup_users(cng$twitter_account[is.na(cng$user_id) & !is.na(cng$twitter_account)])

w <- which(is.na(cng$user_id) & !is.na(cng$twitter_account))

cng$user_id[w] <- nnnnn$user_id[match(tolower(cng$twitter_account[w]), tolower(nnnnn$screen_name))]



cng$screen_name <- tolower(cng$twitter_account)
readr::write_csv(cng, "data/candidate-spreadsheet.csv")



grep('fletch', g$full, value = TRUE, ignore.case = TRUE)

select(dd, user_id, screen_name, id) %>%
  filter(!is.na(id)) %>%
  mutate(screen_name = tolower(screen_name)) %>%
  distinct() %>%
  filter(!user_id %in% cng$user_id) %>%
  arrange(id) %>%
  group_by(id) %>%
  mutate(n = paste0("screen_name", 1 + seq_len(n()))) %>%
  ungroup() %>%
  tidyr::spread(n, screen_name) %>%
  arrange(id)


nad$crp_id <- ifelse(is.na(nad$crp_id), nad$crp_id2, nad$crp_id)

nad[is.na(nad$crp_id), ]

grep("governor|gub", nad$description, ignore.case = TRUE, value = TRUE)


congids <- tfse::read_RDS("data/congress-ids.rds")

nad$user_id %in% congids$user_id


sort(lns)

cands[lns == "whitmire", ] %>%
  as.data.frame()

grep("Whitmer", lns)

cands$crp_id[match(g$full, cands$clean_name)]

sum(is.na(match(g$full, cands$clean_name)))
sum(is.na(match(tolower(g$full), tolower(cands$clean_name))))

filter(cands, is.na(id)) %>%



sum(cng$id %in% cands$google_entity_id)

cands$fec_candidate_id
sum(cng$id %in% cands$fec_candidate_id)

unique(dd$party[grep("Peterson", dd$name)])

sum(tolower(unna) %in% unique(tolower(dd$screen_name)[!is.na(dd$party)]))

s <- dd %>%
  mutate(trump = stringr::str_count(tolower(text), "trump"),
    hashtags = stringr::str_count(text, "(?!<\\S)#\\S+"),
    is_reply = !is.na(reply_to_status_id),
    mentions = stringr::str_count(text, "(?!<\\S)@\\S+") - as.integer(is_reply),
    links = stringr::str_count(text, "https?://\\S+") - as.integer(is_retweet) - as.integer(is_quote)) %>%
  group_by(user_id) %>%
  summarise(
    party = party[1],
    is_retweet = mean(as.integer(is_retweet), na.rm = TRUE),
    is_quote = mean(as.integer(is_quote), na.rm = TRUE),
    is_reply = mean(as.integer(is_reply), na.rm = TRUE),
    tweets = n(),
    trump = mean(trump, na.rm = TRUE),
    hashtags = mean(hashtags, na.rm = TRUE),
    mentions = mean(mentions, na.rm = TRUE),
    links = mean(links, na.rm = TRUE)
  ) %>%
  group_by(party) %>%
  summarise(
    candidates = n(),
    is_retweet = mean(is_retweet, na.rm = TRUE),
    is_quote = mean(is_quote, na.rm = TRUE),
    is_reply = mean(is_reply, na.rm = TRUE),
    tweets = mean(tweets, na.rm = TRUE),
    trump = mean(trump, na.rm = TRUE),
    hashtags = mean(hashtags, na.rm = TRUE),
    mentions = mean(mentions, na.rm = TRUE),
    links = mean(links, na.rm = TRUE)
  ) %>%
  tidyr::gather(var, val, -party) %>%
  tidyr::spread(party, val, fill = 0) %>%
  mutate(DR = D/R) %>%
  arrange(desc(DR)) %>%
  print(n = 100)

select(s, var, D, R, DR)


dd %>%
  group_by(party) %>%
  summarise(tweets = n(),
    users = n_distinct(user_id))

  tbltools::tabsort()


filter(s, !var %in% c("lat", "lng"))

dd[which(duplicated(dd$status_id) | duplicated(dd$status_id, fromLast = TRUE)), ] %>%
  select(status_id, user_id, id:chamber) %>%
  arrange(status_id)

sum(is.na(dd$user_id))



nrow(d)
cng

uuu <- tolower(unique(.s$screen_name[is.na(ids)]))


filter(.s, user_id == "1135486501") %>%
  select(user_id, screen_name)



filter(.s, tolower(screen_name) == "repscottpeters") %>%
  pull(user_id) %>%
  unique()

idlong[idlong$user_id == "1135486501", ]

dapr::vap_lgl(tolower(g$full), ~ any(grepl(.x, unique(tolower(.s$description)))))

Map(grepl, tolower(g$full))


.s$screen_name[match(tolower(g$full_m),
  tolower(.s$description))]

sn3 <- .s$screen_name[match(tolower(g$full_m),
  tolower(.s$description))]
sn3 <- tolower(sn3)

g$screen_name3 <- sn3


g <- mutate(g,
  screen_name3 = ifelse(screen_name != screen_name3 &
      screen_name2 != screen_name3, NA_character_, screen_name3)
)


g$screen_name3 <- NULL

res <- GC()
res
sum(tolower(unique(.s$screen_name)) %in%
  unique(tfse::na_omit(c(g$screen_name, g$screen_name2))))/tfse::n_uq(tolower(.s$screen_name))


g

function(x) {
  unique(select(g, id, user_id, screen_name))
}


tfse::nin(unique(sn2), g$screen_name)
