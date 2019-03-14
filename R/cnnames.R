library(dplyr)

r <- readRDS("data/results.rds")

any_dup <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)



r <- filter(r, !first_name == "" & !last_name == "",
  !(candidate_id %in% c("tyler-64243", "ericson-1158") & is.na(rating)))

r <- filter(r, race_type != "governor")

cng <- readRDS("data/congress-114-115.rds")

r <- r %>%
  mutate(first_last = tolower(paste(first_name, last_name)),
    first_last2 = tolower(paste(sub(" .*", "", first_name), last_name)))


cng <- mutate(cng, f = paste0(state, district)) %>%
  mutate(middle_name = ifelse(is.na(middle_name), " ", paste0(" ", middle_name, " ")),
    full_name = tolower(paste0(first_name, middle_name, last_name)),
    first_last = tolower(paste(first_name, last_name))) %>%
  arrange(desc(congress)) %>%
  filter(!duplicated(full_name))

cng %>%
  transmute(middle_name = ifelse(is.na(middle_name), " ", paste0(" ", middle_name, " ")),
    full_name = tolower(paste0(first_name, middle_name, last_name)),
    first_last = tolower(paste(first_name, last_name)),
    m1 = match(full_name, tolower(r$name_display)),
    m2 = match(first_last, tolower(r$name_display)),
    m3 = match(full_name, tolower(r$first_last)),
    m4 = match(first_last, tolower(r$first_last)),
    m5 = match(full_name, tolower(r$first_last2)),
    m6 = match(first_last, tolower(r$first_last2)),
    m = ifelse(is.na(m1), m2, m1),
    m = ifelse(is.na(m), m3, m),
    m = ifelse(is.na(m), m4, m),
    m = ifelse(is.na(m), m5, m),
    m = ifelse(is.na(m), m6, m)) %>%
  pull(m) -> m


r[grep("murkowski", r$last_name), ]

cng[grep("Ashford", cng$last_name), ]
cng$district


na <- filter(cng, is.na(m)) %>% select(state, district)

  select(full_name, f) %>%
  arrange(f)

filter(cng, state == na$state & disrict == na$district)

filter(cng, f %in% paste0(na$state, na$district)) %>%
  arrange(f) %>%
  select(full_name, f)


library(tfse)

r$first_


gt <- function(x) {
  gt_ <- function(x) {
    x <- gsub(" ", "_", x)
    r <- tryCatch(xml2::read_html(paste0("https://ballotpedia.org/", x)),
      error = function(e) NULL)
    if (is.null(r)) {
      return(NA_character_)
    }
    r <- rvest::html_nodes(r, "p") %>% rvest::html_text(trim = TRUE)
    he <- length(tfse::regmatches_(as.character(r), "\\bhe\\b", drop = TRUE, ignore.case = TRUE))
    she <- length(tfse::regmatches_(as.character(r), "\\bshe\\b", drop = TRUE, ignore.case = TRUE))
    if (he > she) {
      return("man")
    } else if (she > he) {
      return("woman")
    } else {
      NA_character_
    }
  }
  dapr::vap_chr(x, gt_)
}


library(tfse)

cng_ <- read_RDS("~/R/congress_tweets/data/cng-data.rds")

cng <- read_RDS("data/cng.rds")

library(dplyr)


cng %>%
  filter(is.na(gender)) %>%
  tbltools::tabsort(gender2, gender1)


cng <- cng %>%
  mutate(gender = case_when(
    !is.na(gender) ~ gender,
    !is.na(gender1) ~ gender1,
    TRUE ~ gender2
  )) %>%
  select(-gender2, -gender1)


save_RDS(cng, "~/R/congress_tweets/data/cng-data.rds")

cands <- readr::read_csv("~/R/congress_tweets/data/candidates_2018_0921.csv")


np <- rep(NA_character_, nrow(cng))

np1 <- cng_$party[match(cng$fec_candidate_id, cng_$fec_candidate_id)]
np2 <- cands$party[match(cng$fec_candidate_id, cands$fec_candidate_id)]


cands %>%
	mutate(party = case_when(
		grepl("Rep", party, ignore.case = TRUE) ~ "R",
		grepl("dem", party, ignore.case = TRUE) ~ "D",
		TRUE ~ "I"
	)) %>%
	pull(party) %>%
	table()

tbltools::tabsort(cng, gender, party)

nms <- rvest::html_table(xml2::read_html("https://www.fec.gov/campaign-finance-data/all-candidates-file-description/"))

nms <- paste(tolower(nms[[1]][[1]][-1]), collapse = "\t")

w <- readr::read_lines("data/weball18.txt")
w <- gsub("\\|", "\t", w)

tmp <- tempfile()
readr::write_lines(c(nms, w), tmp)

w <- readr::read_tsv(tmp)

caps <- function(x) {
  x <- tolower(x)
  m <- gregexpr("(?<=')\\w|(?<= )i?vi?$|(?<= )i+$|(?<=\\s)\\w|^\\w|(?<=mc)\\w|(?<=-)\\w",
    x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), toupper)
  x <- sub("(?<=J)r$|(?<=S)r$", "r.", x, perl = TRUE)
  gsub("[ ]{2,}", " ", x)
}


w$cand_name <- ifelse(
  w$cand_name == "O'BRIEN CAROL", "O'BRIEN, CAROL", w$cand_name)

w$cand_name <- ifelse(
  w$cand_name == "ADAM DAVIDSON DEMARCO",
  "DEMARCO, ADAM DAVIDSON", w$cand_name)

ln <- sub(",.*", "", w$cand_name)
fn <- sub(".*, ", "", w$cand_name)

w$full_name <- dapr::vap_chr(paste(fn, ln), caps)

paste(fn, ln)[3671]

w$full_name


nin(w$full_name, cng$full_name)
yin(cng$full_name, w$full_name)


i <- match(gsub("\\W", "", tolower(w$full_name)), gsub("\\W", "", tolower(cng$full_name)))
length(i)
nrow()

i <- match(gsub("\\W", "", tolower(cng$full_name)), gsub("\\W", "", tolower(w$full_name)))

names(w)


w <- w %>%
  mutate(party = case_when(
    grepl("Rep", cand_pty_affiliation, ignore.case = TRUE) ~ "R",
    grepl("dem", cand_pty_affiliation, ignore.case = TRUE) ~ "D",
    TRUE ~ "I"
  ))

w$cand_pty_affiliation

cng$ttl_disb <- w$ttl_disb[i]
cng$ttl_receipts <- w$ttl_receipts[i]
#cng$cand_pty_affiliation <- w$party[i]
cng$ttl_indiv_contrib <- w$ttl_indiv_contrib[i]

group_by(cng, party) %>%
	summarise_if(is.numeric, mean, na.rm = TRUE)


with(cng, table(cand_pty_affiliation, party))

table(w$cand_pty_affiliation)
table(cng_$party)

length(i)
length(na_omit(i))
nrow(cng)

length(w$ttl_disb)


select(results, full_name = name_display, percent, nyt_rating, dem_forecast, gop_forecast, third_forecast)

match(cng$full_name, results$name_display)

