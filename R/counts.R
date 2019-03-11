# are_rec <- function(x) vap_lgl(x, is.recursive)
# make_names <- function(x) {
#   uq <- unique(x)
#   for (i in seq_along(uq)) {
#     if (sum(x == uq[i]) > 1) {
#       x[x==uq[i]] <- paste0(x[x==uq[i]], seq_len(sum(x==uq[i])))
#     }
#   }
#   x
# }
#
# counts <- ilap(r$races$counties, ~ {
#   if (nrow(r$races$counties[[.i]]) == 0) {
#     return(tibble::tibble(
#       fips = NA_character_,
#       name = NA_character_,
#       votes = NA_integer_,
#       precincts =  NA_integer_,
#       race_id = r$races$race_id[.i]
#     ))
#   }
#   r$races$counties[[.i]]$race_id <- r$races$race_id[.i]
#   r$races$counties[[.i]]$reporting <- NULL
#   opts <- names(r$races$counties[[.i]]$results)
#   parties <- r$races$candidates[[.i]]$party_id
#   pparties <- make_names(parties)
#   for (i in seq_along(parties)) {
#     r$races$counties[[.i]][[pparties[i]]] <- r$races$counties[[.i]]$results[[i]]
#   }
#   r$races$counties[[.i]]$results <- NULL
#   r$races$counties[[.i]]$race_id <- r$races$race_id[.i]
#   r$races$counties[[.i]]
# })
#
# bind_rows_data(counts, fill = TRUE)