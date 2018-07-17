## load and auto-insert dropbox token
token <- readRDS("~/Dropbox/.r2drop.rds")
assign("token", token, envir = rdrop2:::.dstate)

## function to read RDS
drop_read_rds <- function(file) {
  tmp <- tempfile()
  drop_download(file, tmp, overwrite = TRUE)
  readRDS(tmp)
}

## function to read CSV
drop_read_csv <- function(file) {
  tmp <- tempfile()
  drop_download(file, tmp, overwrite = TRUE)
  readr::read_csv(tmp)
}
