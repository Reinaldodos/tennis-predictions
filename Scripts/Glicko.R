pacman::p_load(tidyverse, data.table, rio, rvest, lubridate)
"Scripts/Fonctions.R" %>% source()

Tourney = "Liste tournois.rds" %>% read_rds()

input =
  "data/" %>%
  list.files(pattern = ".json", full.names = T) %>%
  set_names() %>%
  map(UNJSON) %>%
  bind_rows(.id = "file") %>%
  extract(col = file,
          into = c("file"),
          regex = "data//(.*).json") %>%
  inner_join(x = Tourney, by = "file") %>%
  mutate(Date = tourney_date + ddays(rowid - 1)) %>%
  mutate_all(type.convert)

# Glicko global -----------------------------------------------------------
Elo_global = input %>% GLICKO()
Elo_surface =
  input %>%
  split(f = .$surface) %>%
  map(.f = GLICKO)

list(Elo_global = Elo_global,
     Elo_surface = Elo_surface) %>%
  saveRDS(file = "Elo.rds")
