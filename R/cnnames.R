

library(tfse)

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

cng <- read_RDS("/tmp/cng.rds")

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

w <- readr::read_lines("/Users/mwk/R/congress_tweets/data/weball18.txt")
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

cng$ttl_disb <- w$ttl_disb[i]
cng$ttl_receipts <- w$ttl_receipts[i]
cng$cand_pty_affiliation <- w$party[i]
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

w <- w %>%
	mutate(party = case_when(
		grepl("Rep", cand_pty_affiliation, ignore.case = TRUE) ~ "R",
		grepl("dem", cand_pty_affiliation, ignore.case = TRUE) ~ "D",
		TRUE ~ "I"
	))


