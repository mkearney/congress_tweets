cng <- readRDS("data/cng-final.rds")


d <- fst::read_fst("~/Dropbox/tweets-2017-2018-flat-small.fst")
d <- tibble::as_tibble(d)

## code topics
regex_hc <- paste0(
	"health.?care|obama.?care|affordable\\s?care|\\bACA\\b|",
	"repeal|pre.?existing|prescription.?drugs")
regex_mc <- paste0("caravan|migrant|wall\\b|\\bwall|border")
regex_mt <- paste0("metoo|timesup|me.?too.?move")
regex_im <- paste0("immigra|deport|citizenship|amnesty")
regex_dt <- paste0("trump")

## create/estimate variables
d$health_care <- grepl(regex_hc, d$text, ignore.case = TRUE)
d$migrant_caravan <- grepl(regex_mc, d$text, ignore.case = TRUE)
d$me_too <- grepl(regex_mt, d$text, ignore.case = TRUE)
d$immigration <- grepl(regex_im, d$text, ignore.case = TRUE)
d$trump_mention <- grepl(regex_dt, d$text, ignore.case = TRUE)
d$sentiment <- textfeatures:::sentiment_afinn(d$text)
d$urls_count <- stringr::str_count(d$urls_urls, "http")
d$is_reply <- !is.na(d$reply_to_status_id)
d$years_on_twitter <- as.numeric(difftime(as.POSIXct("2018-11-08"),
	d$account_created_at, units = "days"))/365

## drop some columns
d <- d[grep("account_lang|^profile|^quoted_[^csu]|^retweet_([^csu])|^place|country|location|description|protected|^text$",
	names(d), invert = TRUE)]

fst::write_fst(d, "~/Dropbox/d.fst")

## select id/numeric variables
b <- dplyr::bind_cols(
	dplyr::select(d, user_id, status_id, screen_name, name),
	dplyr::select_if(d, ~ (is.numeric(.) | is.logical(.)) & !all(is.na(.))))

## matching attempt #1
b$full_name2 <- cng$full_name[match(tolower(b$name), tolower(cng$full_name))]
b$full_name <- cng$full_name[match(tolower(b$screen_name), tolower(cng$twitter_account))]
b <- b %>%
	dplyr::mutate(full_name = ifelse(is.na(full_name),
		full_name2, full_name))
b$full_name2 <- NULL

## join on match attempt #1
b <- left_join(b, cng)


## match attempt #2
bre <- paste0("\\b", substr(tolower(cng$first_name), 1, 3),
	".*\\b", tolower(cng$last_name), "\\b")
ii <- !duplicated(bre)
bre <- bre[ii]
fn <- cng$full_name[ii]
bn <- unique(b$name)
un <- unique(b$user_id)
nn <- purrr::map(seq_len(length(bre)), ~ {
	tibble::tibble(full_name = fn[.x],
	  user_id = un[grep(bre[.x], bn, ignore.case = TRUE)])
})
nn <- unique(dplyr::bind_rows(nn))
b$full_name2 <- nn$full_name[match(b$user_id, nn$user_id)]
b <- b %>%
	mutate(full_name = ifelse(is.na(full_name), full_name2, full_name))

b$full_name2 <- NULL

## join new matches
b2 <- b %>%
	filter(is.na(party), !is.na(full_name)) %>%
	select(-c(id:third_forecast)) %>%
	left_join(cng)

## final join
b <- bind_rows(filter(b, !status_id %in% b2$status_id), b2)

## set forecast estimates
b <- b %>%
	dplyr::mutate(
		fcast = dplyr::case_when(
			grepl("Bern.* Sanders", full_name) ~ dem_forecast + third_forecast,
			party == "D" ~ dem_forecast,
			party == "R" ~ gop_forecast,
			is.na(party) ~ NA_real_,
			TRUE ~ third_forecast
		),
		over_perf = (percent/100) - fcast
	)

saveRDS(b, "~/Box Sync/midterm18_twitter/final-full-data.rds")

filter(b, !is.na(full_name)) %>%
  readr::write_csv("~/Box Sync/midterm18_twitter/final-full-data.csv")


s <- b %>%
	group_by(user_id) %>%
	summarise(n_tweet = n(),
		full_name = full_name[1],
		state = state[1],
		fcast = fcast[1],
		n_retweet = sum(is_retweet),
		n_quote = sum(is_quote),
		sentiment = mean(sentiment),
		followers_count = followers_count[1],
		friends_count = friends_count[1],
		favourites_count = favourites_count[1],
		statuses_count = statuses_count[1],
		favorite_count = sum(favorite_count),
		retweet_count = sum(retweet_count),
		listed_count = listed_count[1],
		trump_mention = sum(trump_mention),
		media_count = sum(media_count),
		ext_media_count = sum(ext_media_count),
		urls_count = sum(urls_count),
		health_care = sum(health_care),
		me_too = sum(me_too),
		mig_immig = sum(migrant_caravan | immigration),
		migrant_caravan = sum(migrant_caravan),
		immigration = sum(immigration),
		incumbent = incumbent[1],
		has_incumbent = has_incumbent[1],
		nyt_rating = nyt_rating[1],
		rating = rating[1],
		party = party[1],
		chamber = chamber[1],
		tweet_length = mean(display_text_width),
		dw_nominate = dw_nominate[1],
		gender = gender[1],
		age = as.numeric(difftime(Sys.Date(), date_of_birth[1], units = "days")/365),
		over_perf = over_perf[1],
		percent = percent[1],
	) %>%
	ungroup()

mean1 <- function(x) {
  if (!is.numeric(x)) {
    return(x[1])
  }
  if (is.integer(x)) {
  	as.integer(mean(x, na.rm = TRUE))
  } else {
  	mean(x, na.rm = TRUE)
  }
}

s %>%
  filter(!is.na(fcast)) %>%
  group_by(full_name) %>%
  summarise_all(mean1) -> s







library(ggplot2)

is_t <- function(x) !is.na(x) & x


library(dplyr)

s <- readRDS("data/final-summary-data.rds")


s %>%
	filter(abs(over_perf) < .2) %>%
	mutate(incumbent = is_t(incumbent),
		has_incumbent = is_t(has_incumbent)) %>%
	group_by(incumbent) %>%
	summarise(over_perf = mean(over_perf, na.rm = TRUE))

s <- s %>%
	filter(abs(over_perf) < .25) %>%
	mutate(incumbent = is_t(incumbent),
		has_incumbent = is_t(has_incumbent)) %>%
	filter(party != "I")

## load packages
library(dplyr)
library(ggplot2)

## read data
s <- readRDS("data/final-summary-data.rds")

## functions for transforming predictors
is_intish <- function(x) {
	if (is.integer(x)) return(TRUE)
	if (!is.numeric(x)) return(FALSE)
	all((x %% 1) == 0)
}
should_log <- function(x) {
	if (!is_intish(x)) {
		return(FALSE)
	}
	all(x > 0)
}

## model #1: no interactions
s %>%
  dplyr::select(-c(immigration, migrant_caravan, statuses_count,
  	friends_count, favorite_count, retweet_count,
  	ext_media_count, user_id, percent, rating, nyt_rating, state,
  	full_name, fcast, tweet_length)) %>%
  dplyr::mutate(n_quote = n_quote + 1,
  	favourites_count = favourites_count + 1) %>%
	dplyr::mutate_if(should_log, log10) %>%
	dplyr::mutate(over_perf = over_perf*100) %>%
	lm(over_perf ~ ., data = .) -> m1

## model #2: m1 + party interaction
s %>%
  dplyr::select(-c(immigration, migrant_caravan, statuses_count,
  	friends_count, favorite_count, retweet_count,
  	ext_media_count, user_id, percent, rating, nyt_rating, state,
  	full_name, fcast, tweet_length)) %>%
  dplyr::mutate(n_quote = n_quote + 1,
  	favourites_count = favourites_count + 1) %>%
	dplyr::mutate_if(should_log, log10) %>%
	dplyr::mutate(over_perf = over_perf*100) %>%
	lm(over_perf ~ . +
		trump_mention   * party +
		health_care     * party +
		me_too          * party +
		mig_immig       * party, data = .) -> m2

## model #3: m1 + gender interaction
s %>%
  dplyr::select(-c(immigration, migrant_caravan, statuses_count,
  	friends_count, favorite_count, retweet_count,
  	ext_media_count, user_id, percent, rating, nyt_rating, state,
  	full_name, fcast, tweet_length)) %>%
  dplyr::mutate(n_quote = n_quote + 1,
  	favourites_count = favourites_count + 1) %>%
	dplyr::mutate_if(should_log, log10) %>%
	dplyr::mutate(over_perf = over_perf*100) %>%
	lm(over_perf ~ . +
		trump_mention   * gender +
		health_care     * gender +
		me_too          * gender +
		mig_immig       * gender, data = .) -> m3

## model #4: m1 + m2 + m3
s %>%
  dplyr::select(-c(immigration, migrant_caravan, statuses_count,
  	friends_count, favorite_count, retweet_count,
  	ext_media_count, user_id, percent, rating, nyt_rating, state,
  	full_name, fcast, tweet_length)) %>%
  dplyr::mutate(n_quote = n_quote + 1,
  	favourites_count = favourites_count + 1) %>%
	dplyr::mutate_if(should_log, log10) %>%
	dplyr::mutate(over_perf = over_perf*100) %>%
	lm(over_perf ~ . +
		trump_mention   * party +
		health_care     * party +
		trump_mention   * gender +
		health_care     * gender +
		me_too          * party +
		me_too          * gender +
		mig_immig       * party +
		mig_immig       * gender, data = .) -> m4

## model summaries
summary(m1)
summary(m2)
summary(m3)
summary(m4)

## generate code for html table
h <- list(m1, m2, m3) %>%
	stargazer::stargazer(type = "html", single.row = TRUE) %>%
	gsub("(?<=[^\\d])0\\.", ".", h, perl = TRUE)

## save as models.html
writeLines(h, "docs/models.html")

## view HTML table
browseURL("docs/models.html")

## generate predicted values
s$pred <- s %>%
  dplyr::select(-c(immigration, migrant_caravan, statuses_count,
  	friends_count, favorite_count, retweet_count,
  	ext_media_count, user_id, percent, rating, nyt_rating, state,
  	full_name, fcast, tweet_length)) %>%
  dplyr::mutate(n_quote = n_quote + 1,
  	favourites_count = favourites_count + 1) %>%
	dplyr::mutate_if(should_log, log10) %>%
	predict(m4, newdata = ., type = "response")

## plot to show interaction(s)
s %>%
	dplyr::filter(pred < 5) %>%
	dplyr::mutate(mig_immig = mig_immig + 1) %>%
	ggplot(aes(x = log10(mig_immig), y = pred)) +
	geom_point(aes(fill = party), alpha = .5, size = 2, shape = 21) +
	geom_smooth(method = "lm", aes(color = party)) +
	facet_wrap(~ gender, scales = "free_x") +
	dataviz::theme_mwk(base_size = 14) +
	scale_color_manual(
		values = c(D = "#0022dd", R = "#dd1111")) +
	scale_fill_manual(
		values = c(D = "#0022dd", R = "#dd1111")) +
	labs(title = "Over-performance predicted by immigration mentions, gender, and party",
		x = "Immigration Mentions (log)",
		y = "Predicted Overperformance (%)") +
	ggsave("docs/interaction-mig-plot.png",
		width = 9, height = 5, units = "in")




lm(op ~ )


list.files("data")

cng$f <- with(cng, paste0(state, chamber, district))


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
