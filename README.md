
<!-- README.md is generated from README.Rmd. Please edit that file -->

# congress\_tweets

Collecting tweets posted by members of the United States Congress.

## Data

Data available in shared `congress_tweets` Dropbox folder

``` r
##
link <- rdrop2::drop_share("congress_tweets")
link$url
```

Read and preview the data

``` r
## read and preview data
data <- drop_read_rds("congress_tweets/init-tmls.rds")
tibble::as_tibble(data)
```
