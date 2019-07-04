Fetch_Page <- function(page) {
  print(page)
  page %>% read_html() %>%
    html_nodes(css = ".button-border") %>%
    html_attr(name = "href") %>%
    str_c("https://www.atptour.com", .) %>%
    setdiff("https://www.atptour.com") %>%
    return()
}

FETCH <- function(url) {
  require(rvest)
  print(url)
  HTML = url %>% read_html()
  toto = HTML %>% html_table(fill = T)
  Info_tourney =
    toto[[1]] %>%
    select(X2, surface = X5) %>%
    extract(col = X2,
            into = c("tourney_name"),
            regex = "(.*)\r") %>%
    filter(!is.na(tourney_name))

  Results = toto[[3]] %>% repair_names()
  names(Results) = (1:ncol(Results)-1) %>% str_c("Final", .)

  Results$Final1[1] = toto[[3]] %>% repair_names() %>% names %>% head(1)
  while(any(Results$Final1 == "")) {
    Results =
      Results %>%
      mutate(Final1 = case_when(Final1 == "" ~ lag(Final1),
                                TRUE ~ Final1))
  }

  Results =
    Results %>% filter(Final8 == "H2H") %>%
    select(
      winner_name = Final2,
      loser_name = Final6,
      score = Final7,
      round = Final1
    )

  Dates =
    HTML %>%
    html_nodes(".tourney-dates") %>%
    html_text() %>%
    str_split(pattern = " - ") %>% flatten_chr() %>%
    str_trim() %>%
    ymd()
  Dates = Dates[!is.na(Dates)]
  Debut = min(Dates)

  Results %>% distinct(round) %>% rowid_to_column() %>%
    mutate(rowid=dense_rank(desc(rowid))) %>%
    mutate(Date = Debut+ddays(rowid)-ddays(1)) %>%
    select(-rowid) %>%
    inner_join(x=Results, by="round") %>%
    tidyr::crossing(Info_tourney, .) %>%
    return()
}

REKORD <- function(url) {
  require(jsonlite)
  url %>% FETCH() %>% arrange(Date) %>%
    group_by(tourney_name, surface, round, Date) %>% nest() %>%
    group_by(tourney_name, surface) %>% nest %>%
    jsonlite::write_json(path = str_c("data/",
                                      str_remove_all(string = url, pattern = "[^[:alnum:]]"),
                                      ".json"))
}

UNJSON <- function(file) {
  require(jsonlite)
  require(purrr)
  file %>%
    fromJSON() %>%
    purrr::flatten() %>%
    map_if(is_list, as_tibble) %>%
    map_if(is_tibble, list) %>%
    bind_cols() %>%
    unnest()%>%
    unnest() %>%
    return()
}

GLICKO <- function(input) {
  pacman::p_load(PlayerRatings)
  input %>%
    arrange(Date) %>%
    mutate(Date = as.period(min(Date) %--% Date) %>% day()) %>%
    select(Date, winner_name, loser_name) %>%
    mutate(Score=1) %>%
    glicko2(gamma = 0, tau = 1.2) %>%
    return()
}
