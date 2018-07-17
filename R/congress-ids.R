
## function to convert strings to data frame
keypairs2df <- function(x) {
  x <- strsplit(x, ": ")
  x <- x[lengths(x) == 2]
  if (length(x) == 0) return(NULL)
  var <- map_chr(x, ~ .x[1])
  val <- map(x, ~ .x[2])
  names(val) <- var
  as.data.frame(val, stringsAsFactors = FALSE, row.names = NULL)
}

## link to data base
legs <- httr::GET("https://github.com/unitedstates/congress-legislators/raw/master/legislators-social-media.yaml")

## read, parse, merge into single data frame, and save
cngids <- httr::content(legs, as = "text", encoding = "UTF-8") %>%
  strsplit("\n") %>% .[[1]] %>%
  grep("^#", ., value = TRUE, invert = TRUE) %>%
  paste0(collapse = " ") %>%
  gsub("\\s{2,}", " ", .) %>%
  strsplit("- id: ") %>%
  .[[1]] %>%
  map(~ sub("social: ", "", .x)) %>%
  map(~ strsplit(.x, "(?<!:) ", perl = TRUE)[[1]]) %>%
  map(keypairs2df) %>%
  bind_rows() %>%
  as_tbl() %>%
  mutate(thomas = gsub("'", "", thomas)) %>%
  select(thomas, govtrack, screen_name = twitter, user_id = twitter_id)




cngids$screen_name[cngids$govtrack == "412438" & !is.na(cngids$govtrack)] <- "justinamash"
cngids$user_id[cngids$govtrack == "412438" & !is.na(cngids$govtrack)] <- "233842454"
cngids$screen_name[cngids$screen_name == "reppeteaguilar" & !is.na(cngids$screen_name)] <- "RepPeteAguilar"
cngids$screen_name[cngids$screen_name == "repdinatitus" & !is.na(cngids$screen_name)] <- "RepDinaTitus"


second_accounts$sn1 <- cngall$screen_name[match(second_accounts$sn1, tolower(cngall$screen_name))]
second_accounts$sn2 <- cngall$screen_name[match(second_accounts$sn2, tolower(cngall$screen_name))]
second_accounts$sn2[second_accounts$sn2 == "repdinatitus"] <- "RepDinaTitus"

saveRDS(cngids, "data/congress-ids.rds")
