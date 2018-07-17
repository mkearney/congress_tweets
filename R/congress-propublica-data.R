pp_congress_path <- function(version = "v1", congress = "115", chamber = "senate") {
  glue::glue("https://api.propublica.org/congress/{version}/{congress}/{chamber}/members.json")
}
pp_api_key <- function() {
  httr::add_headers(`X-API-Key` = Sys.getenv("PROPUBLICA_KEY"))
}
pp_get <- function(url) {
  x <- httr::GET(url, pp_api_key())
  httr::warn_for_status(x)
  tryCatch(pp_from_js(x), error = function(e) pp_parse(x))
}


pp_from_js <- function(x) {
  x <- jsonlite::fromJSON(httr::content(x, as = "text", encoding = "UTF-8"), flatten = TRUE)
  if ("results" %in% names(x)) {
    x <- x$results
  }
  if ("members" %in% names(x)) {
    x <- x$members
    while (length(x) == 1 && is.list(x) && !is.data.frame(x)) {
      x <- x[[1]]
    }
  }
  x
}
pp_parse <- function(x) {
  httr::content(x)
}
pps <- pp_get(pp_congress_path(chamber = "senate"))
pph <- pp_get(pp_congress_path(chamber = "house"))
pp_cng_data <- as_tbl(bind_rows(pps, pph))
