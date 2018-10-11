library(mwk)
library(rtweet)

mc <- lists_members(owner_user = "nytpolitics", slug = "midterm-2018-candidates")
uids <- readRDS("data/uids.rds")
uids <- lookup_users(uids)
y <- !duplicated(c(uids$user_id, mc$user_id))
mc <- c(uids$screen_name, mc$screen_name)[y]

d2 <- vector("list", length(mc))

d <- readRDS("data/midterm-candidate-tweets.rds")
pre <- nrow(d)
newest <- as.character(as.Date(sort(d$created_at, decreasing = TRUE)[1]))
ctr <- 0

nrow100 <- function(x) {
  ceiling(NROW(x) / 100) * 100
}

for (i in seq_along(d2)) {
  since_id <- filter(d, user_id == mc[i]) %>%
    arrange(desc(created_at)) %>%
    pull(status_id) %>%
    .[1]
  d2[[i]] <- get_timeline(mc[i], n = 3200,
    since_id = since_id,
    token = bearer_token())
  if (i == length(d2)) break
  ctr <- nrow100(d[[i]]) + ctr
  if (ctr > (900 * 90)) Sys.sleep(60 * 15)
}

d2 <- dplyr::bind_rows(d2)
d <- bind_rows(d2, d)

if (nrow(d) > pre) {
  saveRDS(d, "data/midterm-candidate-tweets.rds")
}
