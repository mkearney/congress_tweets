## CONGRESS RECORDS DATA
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


r <- readr::read_csv("/Users/kearneymw/Downloads/Untitled spreadsheet - Sheet1.csv")



tfse::pbcopy(paste(cng$twitter_account[1:20], collapse = " "))



cands <- readr::read_csv("data/candidates_2018_0921.csv")

grep("id$", names(r), value = TRUE)

x <- cands$clean_name

gsub("(?>[ ])[A-Z]\\.(?=[ ])", "", x, perl = TRUE)


full_nm_id <- function(x) {
  if ("clean_name" %in% names(x)) {
    n <- x$clean_name
  } else if ("full_name" %in% names(x)) {
    n <- x$full_name
  } else if (all(c("last_name", "middle_name", "first_name") %in% names(x))) {
    n <- dapr::dapc(x[c("last_name", "middle_name", "first_name")], ~ {
      ifelse(is.na(.x), "", .x)
    })
    n$middle_name <- ifelse(
      n$middle_name == "", " ", paste0(" ", n$middle_name, " "))
    n <- paste0(n$first_name, n$middle_name, n$last_name)
  } else {
    stop("Must supply clean_name, full_name, or (first|middle|last)_name")
  }
  ## lowercase and trim white space
  n <- tolower(tfse::trim_ws(n))
  ## get rid following period
  n <- gsub("(?<=\\w)\\.(?= )", "", n, perl = TRUE)
  #n <- gsub("(?<= )\\w+\\.", "", n, perl = TRUE)
  ## get rid of dashes
  n <- gsub("-", " ", n)
  ## get rid of middle initials
  n <- gsub("(?<= )[a-z](?= )", "", n, perl = TRUE)
  ## get rid of quoted middle names–dear god it's dumb
  n <- sub(' \"[^"]+\" ', " ", n)
  ## get rid of jr, sr, i+
  n <- gsub(" (jr|sr|i+)\\.?$", "", n, ignore.case = TRUE)
  ## only keep first two letters of each
  n <- gsub("(?<=\\w{2})\\S+", "", n, perl = TRUE)
  n <- gsub(" ", "", n)

  ## BRANCH/CHAMBER
  if ("branch" %in% names(x)) {
    hs <- substr(x$branch, 1, 1)
  } else if ("chamber" %in% names(x)) {
    hs <- substr(x$chamber, 1, 1)
  } else {
    stop("Must supply branch or chamber variable")
  }

  ## STATE
  if ("office_state" %in% names(x)) {
    x$state <- x$office_state
  }
  if (max(nchar(x$state) > 2)) {
    s <- state.abb[match(tolower(state.name), tolower(x$state))]
  } else {
    s <- x$state
  }

  ## DISTRICT
  if (!"district" %in% names(x)) {
    stop("must supply district var")
  }
  d <- x$district
  d <- gsub(".*large.*", "-999", d, ignore.case = TRUE)
  d <- suppressWarnings(as.integer(d))
  d <- sprintf("%02d", d)
  d <- sub("NA", "", d)
  d <- sub("-999", "AL", d)
  s <- ifelse(d == "", s, paste0(s, d))

  ## PARTY
  if (!"party" %in% names(x)) {
    stop("must supply party var")
  }
  p <- x$party
  p <- gsub("gop", "r", p, ignore.case = TRUE)
  p <- ifelse(grepl("^rep$|^republican$|^gop$|^r$", p, ignore.case = TRUE), "r",
    ifelse(grepl("^dem$|^democrat$|^d$", p, ignore.case = TRUE), "d", "i"))
  if (!"state" %in% names(x)) {
    stop("must supply state var")
  }

  paste0(toupper(hs), "_", toupper(s), "_", toupper(p), "_", n)
}




cands$mu_id <- full_nm_id(cands)

cng$mu_id <- full_nm_id(cng)

tfse::n_uq(cng$mu_id)
tfse::n_uq(cands$mu_id)

cng$district
cands$district


any_dup <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)




filter(cng, last_name == "Sensenbrenner") %>% as.data.frame()
cng$mu_id[cng$last_name == "Sensenbrenner"]

cng$congress

cands$cycle
filter(cands, mu_id %in% c("H_FL21_D_lofr", "H_FL22_D_lofr")) %>% as.data.frame()

cng$congress

cng %>%
  filter(!(any_dup(id) & any_dup(last_name) & congress == "114")) -> cng

cng$mu_id2 <- cands$mu_id[match(cng$crp_id, cands$crp_id)]

bind_rows(select(cng, crp_id, mu_id) %>% mutate(dat = "cng"),
  select(cands, crp_id, mu_id) %>% mutate(dat = "cands")) %>%
  unique() %>%
  filter(!is.na(crp_id)) %>%
  tidyr::spread(dat, mu_id) %>%
  mutate(same = cands == cng,
    same = ifelse(is.na(same), FALSE, same)) %>%
  filter(!same)


filter(cands, crp_id == "N00000267")



cng %>%
  select(crp_id, mu_id, mu_id2) %>%
  arrange(crp_id) %>%
  mutate(same = mu_id == mu_id2,
    same = ifelse(is.na(same), FALSE, same)) %>%
  unique() %>%
  filter(!same) %>%
  print(n = 100)

cng[cng$mu_id != cands$mu_id[match(cng$crp_id, cands$crp_id)] & !is.na(cng$mu_id == cands$mu_id[match(cng$crp_id, cands$crp_id)]), ] %>%
  select(mu_id, crp_id, last_name, first_name, chamber, state) %>%
  arrange(crp_id) %>%
  unique()




tfse::nin(cands$mu_id, cng$mu_id)
tfse::nin(cng$mu_id, cands$mu_id)

sum(cng$mu_id %in% cands$mu_id)
sum(cands$mu_id %in% cng$mu_id)

x <- cng[642, ]
as.data.frame(cng[642, ])


tbltools::tabsort(full_nm_id(cands$clean_name))


names(cands)

dapr::vap_int(cands[grep("id$", names(cands), value = TRUE)], ~ sum(is.na(.x)))
cands$fec_candidate_id
