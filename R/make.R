
## toplevel (identifier) data
cng_toplevel <- readr::read_csv("data/congress-toplevel.csv")

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
  }
  tml[[i]] <- get_timeline(cng_toplevel$user_id[i], n = 3200)
  cat(" +")
}

## merge and save
tml <- dplyr::bind_rows(tml)

## load and auto-insert dropbox token
token <- readRDS("~/Dropbox/.r2drop.rds")
assign("token", token, envir = rdrop2:::.dstate)

## save temporary local file
saveRDS(tml, "/tmp/tml.rds")
save_as_csv(tml, "/tmp/tml.csv")

## upload local tmp file to dropbox
rdrop2::drop_upload("/tmp/tml.rds", "congress_tweets/init-tmls.rds")
rdrop2::drop_upload("/tmp/tml.csv", "congress_tweets/init-tmls.csv")

