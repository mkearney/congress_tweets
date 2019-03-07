## read in data sets (you'll have to adjust the paths to these files)
p <- readr::read_csv("data/candidates_2018_0921.csv")
d <- readr::read_csv(tfse::readlines("data/dem_candidates.csv"))
r <- readr::read_csv(tfse::readlines("data/rep_candidates.csv"))

## select columns and rename 'clean_name' to 'candidate'
p2 <- dplyr::select(p, candidate = clean_name, party:branch, url:google_entity_id)

## join the 538 dem and rep data sets
dr <- dplyr::full_join(d, r)

## lowercase the names to better match the propublica data
names(dr) <- tolower(names(dr))

## join with propublica data
dat <- dplyr::full_join(p2, dr)
