ATP = "https://www.atptour.com/en/scores/results-archive"
pacman::p_load(tidyverse, data.table, rio, rvest, lubridate)
"Scripts/Fonctions.R" %>% source()

# Fetch urls --------------------------------------------------------------
Base = "https://www.atptour.com/en/scores/results-archive?"

Types = c(
  "tournamentType=ch",
  "tournamentType=atp",
  "tournamentType=gs",
  "tournamentType=fu",
  "tournamentType=XXI"
) %>% str_c("&", .)

Years = 1915:year(Sys.Date()) %>% str_c("year=", .)

Pages =
  tidyr::crossing(Years, Types) %>%
  mutate(url = str_c(Base, Years, Types)) %>% pull(url)

Tournaments = Pages %>% map_df(Fetch_Page)

Tournaments %>%
  mutate(file = str_remove_all(string = url,
                               pattern = "[^[:alnum:]]")) %>%
  saveRDS(file = "Liste tournois.rds")
