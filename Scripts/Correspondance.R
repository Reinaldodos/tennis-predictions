# Correspondance ----------------------------------------------------------
Elo=Elo_surface$Grass$ratings
Correspondance =
  Players %>%
  set_names() %>%
  map(.f = grep, x=Elo$Player, value=T) %>%
  map(.f =~data.table(Reference=.)) %>%
  bind_rows(.id = "Player")

Tocheck = setdiff(Players, Correspondance$Player)

Correspondance =
  Tocheck %>%
  set_names() %>%
  map(.f = agrep,
      x = Elo$Player,
      value = T) %>%
  map(.f =  ~ data.table(Reference = .)) %>%
  bind_rows(.id = "Player") %>%
  bind_rows(Correspondance)

Tocheck = setdiff(Players, Correspondance$Player)

# Correspondance =
#   Tocheck %>% str_split(pattern = " ") %>% flatten_chr() %>%
#   set_names() %>%
#   map(.f = agrep,
#       x = Elo$Player,
#       value = T) %>%
#   map(.f =  ~ data.table(Reference = .)) %>%
#   bind_rows(.id = "Player") %>%
#   count(Reference) %>%
#   filter(n == max(n)) %>% select(Reference) %>%
#   cbind.data.frame(Player = Tocheck) %>%
#   bind_rows(Correspondance)
#
