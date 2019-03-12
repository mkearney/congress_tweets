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

library(dplyr)

cng <- dplyr::bind_rows(h14, h15, s14, s15)

sn2id <- function(x) {
  dapr::vap_chr(x, ~ {
    if (is.na(.x)) return(NA_character_)
    tw <- rtweet::lookup_users(.x, parse = FALSE)
    tw <- tw[["user_id"]]
    if (length(tw) == 0) {
      return(NA_character_)
    }
    return(tw[1])
  })
}

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


sn2 <- d$screen_name[match(tolower(g$full), tolower(d$name))]
sn2 <- tolower(sn2)

g$screen_name2 <- sn2

g <- mutate(g,
  screen_name2 = ifelse(screen_name == screen_name2, NA_character_, screen_name2)
)

unique(d$description)[grep("Adam Schiff", unique(d$description), ignore.case = TRUE)]

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
ud <- select(filter(d, !duplicated(user_id)), user_id, screen_name, description)

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

did <- d$user_id[match(idlong$screen_name, tolower(d$screen_name))]

idlong$user_id[is.na(idlong$user_id)] <- did[is.na(idlong$user_id)]


idlong <- filter(idlong, !is.na(user_id))

idwide <- idlong %>%
  group_by(id) %>%
  summarise(screen_name = paste(screen_name, collapse = " "),
    user_id = paste(user_id, collapse = " "),
    n = max(n)) %>%
  arrange(desc(n)) %>%
  print(n = 25)


ls(all.names = TRUE)

sum(is.na(idlong$user_id))



dud <- unique(select(d, user_id))
ids <- idlong$id[match(dud$user_id, idlong$user_id)]

dud$id <- ids

d <- left_join(d, dud)
dd <- left_join(d, filter(select(cng, id, party, date_of_birth, gender, state,
  chamber), !duplicated(id)))


s <- dd %>%
  mutate(trump = stringr::str_count(tolower(text), "trump"),
    is_retweet = as.integer(is_retweet),
    mentions = stringr::str_count(text, "@\\S+"),
    is_quote = as.integer(is_quote),
    links = stringr::str_count(text, "https?://\\S+")) %>%
  group_by(user_id) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  ungroup() %>%
  left_join(select(filter(dd, !duplicated(user_id)), user_id, party)) %>%
  group_by(party) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  tidyr::gather(var, val, -party) %>%
  tidyr::spread(party, val, fill = 0) %>%
  .[c(1, 2, 4)] %>%
  mutate(DR = D/R) %>%
  arrange(desc(DR)) %>%
  filter(!grepl("^retweet|^quote", var)) %>%
  print(n = 100)

filter(s, !grepl("lat|lng|^n$|_count", var))

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

uuu <- tolower(unique(d$screen_name[is.na(ids)]))


filter(d, user_id == "1135486501") %>%
  select(user_id, screen_name)



filter(d, tolower(screen_name) == "repscottpeters") %>%
  pull(user_id) %>%
  unique()

idlong[idlong$user_id == "1135486501", ]

dapr::vap_lgl(tolower(g$full), ~ any(grepl(.x, unique(tolower(d$description)))))

Map(grepl, tolower(g$full))


d$screen_name[match(tolower(g$full_m),
  tolower(d$description))]

sn3 <- d$screen_name[match(tolower(g$full_m),
  tolower(d$description))]
sn3 <- tolower(sn3)

g$screen_name3 <- sn3


g <- mutate(g,
  screen_name3 = ifelse(screen_name != screen_name3 &
      screen_name2 != screen_name3, NA_character_, screen_name3)
)


g$screen_name3 <- NULL

res <- GC()
res
sum(tolower(unique(d$screen_name)) %in%
  unique(tfse::na_omit(c(g$screen_name, g$screen_name2))))/tfse::n_uq(tolower(d$screen_name))


g

function(x) {
  unique(select(g, id, user_id, screen_name))
}


tfse::nin(unique(sn2), g$screen_name)
