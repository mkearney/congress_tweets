## CNN and CSPAN list IDs
listids <- c("104306527", "34179516")

cngall <- map(listids, lists_members)
cngall <- bind_rows(cngall)

cngids <- readRDS("data/congress-ids.rds")


cleanup_names <- function(x) {
  x <- tolower(x)
  x <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
  x <- x[!x %in% c("&", "and", "sen", "senator", "rep", "representative", "", "sr", "jr", "ii", "iii",
    "senate", "house", "dem", "dems", "democrat", "democrats", "republican", "gop", "republicans")]
  if (length(x) == 0) return(NA_character_)
  x <- x[nchar(x) > 1L]
  if (length(x) == 0) return(NA_character_)
  if (any(x[-length(x)] %in% "mc")) {
    x[which(x == "mc") + 1] <- paste0("mc", x[which(x == "mc") + 1])
    x <- x[x == "mc"]
  }
  x
}

nms <- strsplit(cngall$name, "\\s+|,\\s{0,}")
nms[lengths(nms) == 1] <- unlist(nms[lengths(nms) == 1]) %>%
  strsplit("(?<=\\S)(?=[[:upper:]])", perl = TRUE)
nms <- nms %>%
  map(cleanup_names) %>%
  map(~ if (length(.x) == 1) return(c(.x, NA_character_, NA_character_)) else
    if (length(.x) == 2) return(c(.x, NA_character_)) else .x) %>%
  map(sort)

cngall$first_name <- map_chr(nms, 1)
cngall$second_name <- map_chr(nms, 2)
cngall$third_name <- map_chr(nms, 3)


psul <- function(x, n = NULL, rev = FALSE) {
  x <- sort(unique(tolower(x)))
  if (!is.null(n) && n < length(x)) {
    if (rev) {
      x <- x[c(1:(n - 1), length(x))]
    } else {
      x <- x[1:n]
    }
  }
  paste(x, collapse = " ")
}
psul(cngall$name)

second_accounts <- filter(cngall, !duplicated(user_id)) %>%
  select(user_id, screen_name, first_name, second_name, third_name, location, description) %>%
  full_join(cngids, by = c("user_id", "screen_name")) %>%
  filter(!is.na(screen_name)) %>%
  arrange(first_name, second_name, third_name) %>%
  select(screen_name:third_name, user_id) %>%
  tidyr::gather(pos, name, -screen_name, -user_id) %>%
  filter(!is.na(name)) %>%
  group_by(screen_name) %>%
  mutate(nm1 = psul(name, 1),
    nm2 = psul(name, 2),
    nm2r = psul(name, 2, rev = TRUE)) %>%
  ungroup() %>%
  select(screen_name, user_id, nm2, nm2r) %>%
  tidyr::gather(var, nm, -screen_name, -user_id) %>%
  select(-var) %>%
  distinct() %>%
  group_by(nm) %>%
  summarise(sn = psul(screen_name), n = n()) %>%
  filter(n > 1) %>% select(-n) %>%
  filter(!nm %in% c("jec", "means ways", "energy", "commerce energy", "armed services", "affairs foreign", "affairs committee")) %>%
  mutate(sn1 = sub("\\s.*", "", sn), sn2 = sub(".*\\s", "", sn)) %>%
  select(-sn)


cngsns <- filter(cngall, !duplicated(user_id)) %>%
  filter(!user_id %in% cngids$user_id) %>%
  select(screen_name) %>%
  mutate(sn2 = second_accounts$sn2[match(tolower(screen_name), second_accounts$sn1)],
    sn1 = second_accounts$sn1[match(tolower(screen_name), second_accounts$sn2)],
    sn = ifelse(is.na(sn2), sn1, sn2)) %>%
  select(-sn1, -sn2) %>%
  filter(!is.na(sn)) %>%
  select(screen_name2 = screen_name, screen_name = sn) %>%
  full_join(cngids, by = "screen_name") %>%
  select(govtrack, thomas, user_id, screen_name, screen_name2)

sn2 <- lookup_users(cngsns$screen_name2)

cngsns$user_id2 <- c(sn2$user_id[1:18], rep(NA_character_, (nrow(cngsns) - 18)))

cngsns_final <- filter(cngsns, is.na(user_id2)) %>% select(govtrack:screen_name)

cngsns_final <- filter(cngsns, !is.na(screen_name2)) %>%
  select(-user_id, -screen_name) %>%
  select(govtrack, thomas, user_id = user_id2, screen_name = screen_name2) %>%
  bind_rows(cngsns_final, .)

select(cngsns, govtrack, thomas, user_id, screen_name, user_id2, screen_name2)



full_join(select(cngsns_final, govtrack_id = govtrack, user_id), pp_cng_data, by = "govtrack_id") %>%
  select(user_id, govtrack = govtrack_id, title, short_title, first_name:party, state) %>%
  mutate(date_of_birth = as.Date(date_of_birth)) %>%
  readr::write_csv("data/congress-toplevel.csv")
