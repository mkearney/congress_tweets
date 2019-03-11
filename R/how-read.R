
## read flat/small tweets
d <- readRDS("data/tweets-2017-2018-flat-small.rds")
d <- tibble::as_tibble(fst::read_fst("data/tweets-2017-2018-flat-small.fst"))
