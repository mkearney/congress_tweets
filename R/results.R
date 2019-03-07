wrap_url("https://int.nyt.com/applications/elections/2018/forecast/2018-11-06/senate/summary.json")

devtools::install("~/R/tfse")

r <- "https://int.nyt.com/applications/elections/2018/api/1/races/2018-11-06.json" %>% jsonlite::fromJSON()
str(r$races, 1)
str(r, 1)
r <- tibble::as_tibble(r)
r <- r$races

save_RDS(r, "data/results-2018.rds")



library(dplyr)

dem <- readr::read_csv(tfse::readlines("data/dem_candidates.csv"))
names(dem) <- gsub(" ", "_", tolower(names(dem)))
dem$race_type <- tolower(dem$office_type)
dem$party_id <- "democrat"

left_join(dplyr::select(dem, state, race_type, party_id, partisan_lean),
  dplyr::select(r, state = state_id, race_type, party_id, percent)) %>%
  dplyr::filter(!is.na(percent)) %>%
  dplyr::mutate(perf = percent + partisan_lean) %>%
  dplyr::arrange(perf) %>%
  print(n = 100)



readr::read_csv("data/candidates_2018_0921.csv")

r <- ilap(r$candidates, ~ {
  .x <- r$candidates[[.i]]
  .x$race_id <- r$race_id[.i]
  #.x$ranked_choice <- NULL
  repos_front(.x, race_id)
}) %>%
  bind_rows_data(fill = TRUE) %>%
  #filter(first_name != "")
  left_join(select(r, -candidates, -votes, -runoff, -party_id)) %>%
  select(-ranked_choice)



wrap_url("https://int.nyt.com/applications/elections/2018/api/1/live-reporters/2018-11-06.json")

"https://int.nyt.com/applications/elections/2018/api/1" %P%
  "/live-reporters/2018-11-06.json" %>% jsonlite::fromJSON() -> r

pat <- paste(gsub(" ", "-", tolower(state.name)), collapse = "|")
state_results <- unique(grep("results.*(" %P% pat %P% ").*elections\\.html", r$url, value = TRUE))

str(r, 2)
r$url
