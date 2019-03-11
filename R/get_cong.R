congress <- 115

get_cong_data <- function(congress, chamber) {
  res <- httr::GET(
    "https://api.propublica.org/congress/v1/{congress}/{chamber}" %PP%
      "/members.json",
    httr::add_headers("X-API-Key" = Sys.getenv("PROPUBLICA_KEY")))
  d <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  d <- d$results$members[[1]]
  d$chamber <- chamber
  d$congress <- as.character(congress)
  tibble::as_tibble(d)
}

h14 <- get_cong_data(114, "house")
s14 <- get_cong_data(114, "senate")
h15 <- get_cong_data(115, "house")
s15 <- get_cong_data(115, "senate")


cng <- dplyr::bind_rows(h14, h15, s14, s15)

save_RDS(cng, "data/congress-114-115.rds")
