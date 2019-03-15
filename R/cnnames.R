library(dplyr)

r <- readRDS("data/results.rds")

any_dup <- function(x) duplicated(x) | duplicated(x, fromLast = TRUE)



r <- filter(r, !first_name == "" & !last_name == "",
  !(candidate_id %in% c("tyler-64243", "ericson-1158") & is.na(rating)))

r <- filter(r, race_type != "governor")

#cng <- readRDS("data/congress-114-115.rds")
#list.files("data", pattern = "^c")

cng <- readRDS("data/cng-data.rds")

sum(duplicated(cng$full_name))

paste_fml <- function(f, m, l) {
  m <- ifelse(is.na(m), " ", paste0(" ", m, " "))
  f[is.na(f)] <- ""
  l[is.na(l)] <- ""
  fml <- paste0(f, m, l)
  fml[grepl("^\\s{0,}$", fml)] <- NA_character_
  fml
}

paste_fl <- function(f, l) {
  f[is.na(f)] <- ""
  l[is.na(l)] <- ""
  fl <- paste(f, l)
  fl[grepl("^\\s{0,}$", fl)] <- NA_character_
  fl
}


r <- r %>%
  mutate(first_last = paste_fl(first_name, last_name),
    first_last2 = paste_fl(sub(" .*", "", first_name), last_name))

r$first_last

names(cng)

cng %>%
  transmute(first_last = paste_fl(first_name, last_name),
    m1 = match(tolower(full_name), tolower(r$name_display)),
    m2 = match(tolower(first_last), tolower(r$name_display)),
    m3 = match(tolower(full_name), tolower(r$first_last)),
    m4 = match(tolower(first_last), tolower(r$first_last)),
    m5 = match(tolower(full_name), tolower(r$first_last2)),
    m6 = match(tolower(first_last), tolower(r$first_last2)),
    m = ifelse(is.na(m1), m2, m1),
    m = ifelse(is.na(m), m3, m),
    m = ifelse(is.na(m), m4, m),
    m = ifelse(is.na(m), m5, m),
    m = ifelse(is.na(m), m6, m)) %>%
  pull(m) -> m


na <- filter(cng, is.na(m)) %>% select(full_name, state, chamber, district) %>%
	mutate(f = paste0(state, chamber, district))

cng %>%
	mutate(f = paste0(state, chamber, district)) %>%
	filter(f %in% na$f) %>%
  arrange(f) %>%
  select(full_name, f, last_name) %>%
  pull(last_name)-> ln

r[grep(paste(ln, collapse = "|"), r$last_name), ] %>%
	select(last_name, name_display, state_id) %>%
	filter(last_name %in% cng$last_name) %>%
	left_join(select(cng, last_name, full_name, state)) %>%
	distinct() %>%
	filter(state_id == state) %>%
	filter(full_name %in% na$full_name) %>%
	select(name_display, full_name) -> rm


r$name_display[match(rm$name_display,
	r$name_display)] <- cng$full_name[match(rm$full_name, cng$full_name)]







cng %>%
  transmute(first_last = paste_fl(first_name, last_name),
    m1 = match(tolower(full_name), tolower(r$name_display)),
    m2 = match(tolower(first_last), tolower(r$name_display)),
    m3 = match(tolower(full_name), tolower(r$first_last)),
    m4 = match(tolower(first_last), tolower(r$first_last)),
    m5 = match(tolower(full_name), tolower(r$first_last2)),
    m6 = match(tolower(first_last), tolower(r$first_last2)),
    m = ifelse(is.na(m1), m2, m1),
    m = ifelse(is.na(m), m3, m),
    m = ifelse(is.na(m), m4, m),
    m = ifelse(is.na(m), m5, m),
    m = ifelse(is.na(m), m6, m)) %>%
  pull(m) -> m





rb <- r[tfse::na_omit(m), ] %>%
	select(race_id:candidate_key, party_id:percent, nyt_rating:third_forecast)

cng %>%
	filter(!is.na(m)) %>%
	bind_cols(rb) -> cng

table(cng$chamber)

saveRDS(cng, "data/cng-final.rds")

tl <- readr::read_csv("https://github.com/fivethirtyeight/data/raw/master/primary-candidates-2018/dem_candidates.csv")
names(tl)  <- tolower(gsub(" ", "_", gsub("[[:punct:]]", "", names(tl))))
rr <- readr::read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/primary-candidates-2018/rep_candidates.csv")
names(rr)  <- tolower(gsub(" ", "_", gsub("[[:punct:]]", "", names(rr))))



tl <- filter(tl, office_type %in% c("Representative", "Senator"))
tl <- filter(tl, primary_status != "Lost")

tl <- tl %>%
	mutate(district = gsub(".* (?=\\d)", "", district, perl = TRUE),
		district = ifelse(grepl("At-large", district), "At-Large", district),
		district = ifelse(grepl("Senate", district), NA_character_, district),
		chamber = case_when(
			grepl("Represent", office_type) ~ "house",
			grepl("Senat", office_type) ~ "senate",
			TRUE ~ NA_character_),
		f = paste0(state, chamber, district))

rr <- filter(rr, office_type %in% c("Representative", "Senator"))
rr <- filter(rr, primary_status != "Lost")

rr <- rr %>%
	mutate(district = gsub(".* (?=\\d)", "", district, perl = TRUE),
		district = ifelse(grepl("At-large", district), "At-Large", district),
		district = ifelse(grepl("Senate", district), NA_character_, district),
		chamber = case_when(
			grepl("Represent", office_type) ~ "house",
			grepl("Senat", office_type) ~ "senate",
			TRUE ~ NA_character_),
		f = paste0(state, chamber, district))

d <- fst::read_fst("/Users/mwk/Dropbox/tweets-2017-2018-flat-small.fst")
d <- tibble::as_tibble(d)


hc <- paste0("health.?care|Obamacare|affordable\\s?care|ACA\\b|",
	"repeal|pre.?existing|prescription|drugs")
mc <- paste0("caravan|migrant|\\bwall|border|immigra")
mt <- paste0("metoo|timesup|me too move")

hcv <- grepl(hc, d$text, ignore.case = TRUE)
mcv <- grepl(mc, d$text, ignore.case = TRUE)
m2v <- grepl(mt, d$text, ignore.case = TRUE)
mtv <- grepl("trump", d$text, ignore.case = TRUE)
snt <- sentiment_afinn(d$text)
d$urls_count <- stringr::str_count(d$urls_urls, "http")
d$health_care <- hcv
d$migrant_caravan <- mcv
d$me_too <- m2v
d$mentions_trump <- mtv
d$sentiment <- snt

fst::write_fst(d, "~/Dropbox/d.fst")

b <- bind_cols(select(d, user_id, status_id, screen_name, name, location),
	select_if(d, ~ (is.numeric(.) | is.logical(.)) & !all(is.na(.))))


b$full_name <- cng$full_name[match(tolower(b$screen_name), tolower(cng$twitter_account))]
#b <- filter(b, !is.na(full_name))
b <- left_join(b, cng)



grep("Bern.* Sanders", s$full_name, value= TRUE)

b <- b %>%
	mutate(
		fcast = case_when(
			grepl("Bern.* Sanders", full_name) ~ dem_forecast + third_forecast,
			party == "D" ~ dem_forecast + third_forecast,
			party == "R" ~ gop_forecast + third_forecast,
			is.na(party) ~ NA_real_,
			TRUE ~ third_forecast
		),
		over_perf = (percent/100) - fcast
		#party = ifelse(party_id %in% c("democrat", "republican"), party_id, "third")
	)

	#mutate(party = factor(party, levels = c("republican", "democrat", "third"))) %>%


saveRDS(s, "~/Dropbox/s.rds")

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
		mentions_trump = sum(mentions_trump),
		media_count = sum(media_count),
		ext_media_count = sum(ext_media_count),
		urls_count = sum(urls_count),
		health_care = sum(health_care),
		me_too = sum(me_too),
		migrant_caravan = sum(migrant_caravan),
		incumbent = incumbent[1],
		has_incumbent = has_incumbent[1],
		nyt_rating = nyt_rating[1],
		rating = rating[1],
		party = party[1],
		#chamber = chamber[1],
		tweet_length = mean(display_text_width),
		gender = gender[1],
		age = as.numeric(difftime(Sys.Date(), date_of_birth[1], units = "days")/365),
		over_perf = over_perf[1],
		percent = percent[1],
	) %>%
	ungroup()


options(width = 80)

nnms <- function(s) substr(sub("\\S{4,}.*_", "",
	sub("_count", "", names(s))), 1, 9)


ratings_factor <- function(x) factor(x, levels = c("Solid D", "Likely D", "Lean D", "Tossup", "Lean R", "Likely R", "Solid R"))

s %>%
	select(-user_id) %>%
	mutate(rating = ratings_factor(rating)) %>%
	model.matrix(~ -1 + ., data = .) %>%
	cor() %>%
	.[-1, -1] %>%
	tibble::as_tibble(rownames = "var") %>%
	tbltools::repos_front(var, op) %>%
	arrange(desc(abs(op))) %>%
	select(var, op, percent) %>%
	print(n = 100)

table(cng$party)

filter(cng, (dem_forecast > 0.9 & party == "D") |
 	(gop_forecast > 0.9 & party == "R") |
 	(third_forecast > 0.9 & party == "I")) %>%
	select(full_name, state, district, party, percent,
		dem_forecast, gop_forecast, third_forecast, f) -> ccccc



filter(cng, f %in% ccccc$f) %>%
	select(f, state, district, party, percent,
		dem_forecast, gop_forecast, third_forecast) %>%
	arrange(f)


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


s %>%
	filter(party != "I") %>%
	select(full_name, state, party, percent, fcast,over_perf) %>%
	arrange(over_perf) %>%
	print(n= 50)

s %>%
	#filter(party != "third") %>%
	#mutate(rating = ratings_factor(rating)) %>%
s$party <- as.character(s$party)

library(ggplot2)

s %>%
	filter(party != "I") %>%
	ggplot(aes(x = party, y= over_perf)) +
	geom_boxplot()

range(s$over_perf, na.rm = TRUE)

s %>%
	filter(abs(over_perf) < .5, percent > 0) %>%
	mutate(incumbent = is_t(incumbent),
		has_incumbent = is_t(has_incumbent)) %>%
	group_by(incumbent) %>%
	summarise(over_perf = mean(over_perf, na.rm = TRUE))
with(s, cor(over_perf, as.integer(incumbent)))

is_t <- function(x) !is.na(x) & x

s %>%
	filter(abs(over_perf) < .45) %>%
	mutate(incumbent = is_t(incumbent),
		has_incumbent = is_t(has_incumbent)) %>%
	filter(party != "I", percent> 0) %>%
	select(-c(friends_count:listed_count,
		ext_media_count, tweet_length,
		user_id, percent, rating, nyt_rating, state, full_name, fcast)) %>%
	mutate(over_perf = over_perf*100) %>%
	lm(over_perf ~ . + migrant_caravan*party +
		health_care*party + me_too*party, data = .) -> regmod

summary(regmod)

rockchalk::outreg(regmod, type = "html", tight = FALSE)


	split(.$party) %>%
	purrr::map(function(x) lm(op ~ ., data = select(x, -party, -incumbent, -gender))) %>%
	purrr::map(summary)
#	lm(op ~ ., data = .) %>%
	summary()

b %>%
	mutate_if(is.logical, as.numeric) %>%
	group_by(party) %>%
	summarise_if(is.numeric, mean, na.rm = TRUE) %>%
	tidyr::gather(var, val, -party) %>%
	tidyr::spread(party, val) %>%
	mutate(dr = democrat / republican) %>%
	arrange(desc(dr)) %>%
	filter(!grepl("uncontested|runoff|protected|retweet_[^c]|quoted_|at_large", var)) %>%
	print(n = 50)



lm(op ~ )


list.files("data")

cng$f <- with(cng, paste0(state, chamber, district))
tl$f

tfse::n_uq(cng$f)

tl$veteran

names(tl)


match(tl$f,rr$f)
match(rr$f,tl$f)

match()
tl$partisan_lean
intersect(names(rr), names(tl))


sum(is.na(match(cng$f, .tl$f)))
sum(is.na(cng$partisan_lean))
cng$partisan_lean <- tl$partisan_lean[match(cng$f, tl$f)]

arrange(cng, f) %>%
	select(partisan_lean, f) %>%
	print(n = 100)

dr <- rbind(tl[names(rr)], rr)

match(tl$f, cng$f)
match(rr$f, cng$f)

match(cng$f, tl$f)

select(tl, state, district)

names(tl)
match(tl$candidate)

#tl <- tfse::read_RDS("data/cng-data.rds")

tl[match(cng$govtrack_id, tl$govtrack), ]

names(cng)
cng$state

list.files("data", pattern = "^c")

readRDS("data/congress-ids		")

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

