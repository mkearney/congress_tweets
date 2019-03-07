## load packages
library(kmw)
library(rtweet)

## nytimes' politics list
mc <- lists_members(owner_user = "nytpolitics", slug = "midterm-2018-candidates")

## read all user IDs
uids <- readRDS("data/uids.rds")

## unique midterm candidate IDs
mc <- unique(c(uids, mc$user_id))

## initialize new data object
d2 <- vector("list", length(mc))

## read in data
d <- readRDS("data/midterm-candidate-tweets-ids.rds")

if (ncol(d) > 2) {
  ## arrange by timestamp, select ID columns, and filter first obs for each user
  d <- d %>%
    arrange_data(decr(created_at)) %>%
    select_data(user_id, status_id) %>%
    filter_data(!duplicated(user_id))
}

## initialize counters
ctr <- 0
j <- 1

## counter
nrow100 <- function(x) {
  ceiling(NROW(x) / 100) * 100
}


## the for loop
for (i in seq_along(d2)) {
  ## get since_id param
  since_id <- d %>%
    filter_data(user_id == mc[i]) %>%
    pull_data(status_id)

  ## coerce since_id to appropriate value/length
  if (length(since_id) == 0) {
    since_id <- NULL
  }
  since_id <- since_id[1]

  ## get tweets from timeline since most recent previously collected tweet
  d2[[i]] <- get_timeline(
    mc[i], n = 3200,
    since_id = since_id,
    token = bearer_token()
  )

  ## update message
  print_complete(paste0(mc[i]))

  ## if final iteration, skip counter updates
  if (i == length(d2)) break

  ## update rate-limit counter
  ctr <- nrow100(d2[[i]]) + ctr

  ## update global counter
  if (ctr > (180 * 100 * j)) {
    print_start("Sleeping on rate-limit cooldown...")
    Sys.sleep(60 * 15)
    print_complete("All waked up!")
    j <- j + 1
  }
}

## collapse into single df
d2 <- bind_rows_data(d2, fill = FALSE)

## save data
save_as <- paste0("midterm-candidate-tweets-", Sys.Date(), ".rds")
save_RDS(d2, file.path("data", save_as))

## arrange by timestamp, select ID columns, bind with d, and filter first obs
d <- d2 %>%
  arrange_data(decr(created_at)) %>%
  select_data(user_id, status_id) %>%
  bind_rows_data(d) %>%
  filter_data(!duplicated(user_id))

## save ids data
saveRDS(d, "data/midterm-candidate-tweets-ids.rds")
