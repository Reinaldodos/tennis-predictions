ATP = "https://www.atptour.com/en/scores/results-archive"
pacman::p_load(tidyverse, data.table, rio, rvest, lubridate)
"Scripts/Fonctions.R" %>% source()

# Fetch urls --------------------------------------------------------------
Base = "https://www.atptour.com/en/scores/results-archive?"

Types = c(
  "tournamentType=ch",
  "tournamentType=atpgs",
  "tournamentType=fu",
  "tournamentType=XXI"
) %>% str_c("&", .)

Years = 1915:year(Sys.Date()) %>% str_c("year=", .)

Pages =
  tidyr::crossing(Years, Types) %>%
  mutate(url = str_c(Base, Years, Types)) %>% pull(url)

URLS = Pages %>% map(Fetch_Page) %>% flatten_chr()

# Done or to do? ----------------------------------------------------------
Done = "data/" %>% list.files(pattern = "json") %>% str_remove_all(pattern = ".json")

URLS =
  data.table(url = URLS) %>%
  mutate(file = str_remove_all(string = url,
                               pattern = "[^[:alnum:]]")) %>%
  filter(!file %in% Done) %>% pull(url)

safe_REKORD = safely(REKORD)
URLS %>% walk(.f = safe_REKORD)

