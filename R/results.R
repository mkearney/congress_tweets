library(kmw)
library(dplyr)

## (senate/house) forecast data
"https://int.nyt.com/applications/elections/2018/forecast/" %P%
  "2018-11-06/" %P% c("senate", "house") %P% "/summary.json" %>%
  lap(jsonlite::fromJSON) -> fcasta

## results data
"https://int.nyt.com/applications/elections/2018/api/1/races/" %P%
  "2018-11-06.json" %>%
  jsonlite::fromJSON() -> r

## candidates data
cands <- ilap(r$races$candidates, ~ {
  r$races$candidates[[.i]]$race_id <- r$races$race_id[.i]
  r$races$candidates[[.i]]$ranked_choice <- NULL
  r$races$candidates[[.i]]$order <- NULL
  r$races$candidates[[.i]]
})
cands <- repos_front(bind_rows_data(cands), race_id)

## cleanup
r$races$counties <- NULL
r$races$candidates <- NULL
r$races$townships <- NULL
races <- tibble::as_tibble(r$races)
races$party_id <- NULL
races$runoff <- NULL
names(races)[names(races)=="votes"] <- "votes_total"

## results data (join candidates with race info)
results <- left_join(cands, races)
results <- filter(results, race_type != "ballot-measure")

## select columns
results %>%
  select(race_id, candidate_id:percent, electoral_votes, race_type,
    office, nyt_rating, state_id:uncontested,
    result, result_source, votes_total,
    precincts_total, rating, incumbent_party) -> results

## forecast data set
fcasts <- rbind(select(fcasta[[1]]$races, race_id,
  dem_forecast = democrat_estimate,
  gop_forecast = republican_estimate,
  third_forecast = all_other_estimate) %>%
  as_tibble(),
  select(fcasta[[2]]$races, race_id, dem_forecast = democrat_estimate,
    gop_forecast = republican_estimate,
    third_forecast = all_other_estimate) %>%
  as_tibble())

## join with results
results <- left_join(results, fcasts)

## races, cands, forecasts
save_RDS(races, "data/races.rds")
save_RDS(cands, "data/cands.rds")
save_RDS(fcasts, "data/fcasts.rds")

## results (joined: races, cands, fcasts)
save_RDS(results, "data/results.rds")

## vote shares of two major parties + third party
select(results, race_id, percent, party_id) %>%
  arrange(race_id, decr(percent)) %>%
  group_by(race_id) %>%
  mutate(party_id = ifelse(party_id %in% c("republican", "democrat"),
    party_id, "third")) %>%
  group_by(race_id, party_id) %>%
  summarise(percent = sum(percent)) %>%
  ungroup() %>%
  tidyr::spread(party_id, percent) %>%
  right_join(races) %>%
  .[names(.) %in% c("democrat", "republican", "third", names(results))] %>%
  select(-result) -> three

## save three party vote share
save_RDS(three, "../data/races-three-parties.rds")

## actual results
results %>%
  select(race_id, party_id, percent) %>%
  mutate(party_id = ifelse(party_id %in% c("republican", "democrat"),
    party_id, "third")) %>%
  group_by(race_id, party_id) %>%
  summarise(percent = sum(percent)) %>%
  ungroup() %>%
  tidyr::spread(party_id, percent) %>%
  right_join(races) %>%
  .[names(.) %in% c("democrat", "republican", "third", names(results))] %>%
  select(-result) -> three_actual

save_RDS(three_actual, "../data/races-three-actual-parties.rds")
