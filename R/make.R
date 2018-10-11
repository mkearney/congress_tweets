
## toplevel (identifier) data
library(readr)
cols <- cols(
  user_id = col_character(),
  govtrack = col_character(),
  title = col_character(),
  short_title = col_character(),
  first_name = col_character(),
  middle_name = col_character(),
  last_name = col_character(),
  suffix = col_character(),
  date_of_birth = col_date(format = ""),
  gender = col_character(),
  party = col_character(),
  state = col_character()
)

cng_toplevel <- readr::read_csv("data/congress-toplevel.csv", col_types = cols)
cng_toplevel <- dplyr::filter(cng_toplevel, !is.na(user_id))



## load and auto-insert dropbox token
token <- readRDS("~/Dropbox/.r2drop.rds")
assign("token", token, envir = rdrop2:::.dstate)
o <- rdrop2::drop_download("congress_tweets/tml.rds")

## load rtweet
library(rtweet)

## initalize output vector
tml <- vector("list", nrow(cng_toplevel))

## set timer for rate limit reset
rl_reset <- Sys.time() + 60 * 15 + 10

## loop through to get tweets
for (i in seq_along(tml)) {
  if (i %% 56 == 0) {
    s <- as.numeric(difftime(rl_reset, Sys.time(), units = "secs"))
    if (s < 0) s <- 1
    message("Sleeping for ", round(s / 60, 1), " minutes ")
    Sys.sleep(s)
    rl_reset <- Sys.time() + 60 * 15 + 10
  }
  tml[[i]] <- get_timeline(cng_toplevel$user_id[i], n = 3200)
  cat("+")
}

## merge and save
tml <- dplyr::bind_rows(tml)

## save temporary local file
saveRDS(tml, "/tmp/tml.rds")
save_as_csv(tml, "/tmp/tml.csv")

## upload local tmp file to dropbox
rdrop2::drop_upload("/tmp/tml.rds", "congress_tweets")
rdrop2::drop_upload("/tmp/tml.csv", "congress_tweets")
