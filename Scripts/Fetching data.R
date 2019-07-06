pacman::p_load(tidyverse, data.table, rio, rvest, lubridate)
"Scripts/Fonctions.R" %>% source()

# Done or to do? ----------------------------------------------------------
Done = "data/" %>% list.files(pattern = "json") %>% str_remove_all(pattern = ".json")

URLS =
  "Liste tournois.rds" %>% read_rds() %>%
  filter(!file %in% Done,
         url != "https://www.atptour.com")

safe_REKORD = safely(REKORD)
URLS$url %>%
  walk(.f = safe_REKORD)
