pacman::p_load(tidyverse, data.table, rio)
"Scripts/Fonctions.R" %>% source()
Tourney = "Liste tournois.rds" %>% read_rds()
Inprocess =
  Tourney %>% filter(str_detect(string = url, pattern = "current"))

Inprocess %>% pull(file) %>%
  map(.f = list.files,
      path = "data/",
      full.names = T) %>%
  flatten_chr() %>% unlink()

Inprocess$url %>% walk(safely(REKORD))
