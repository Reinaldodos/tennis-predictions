Fetch_result <- function(result) {
  require(rvest)
  tourney_name = result %>% html_nodes(css = ".tourney-title") %>% html_text() %>% str_trim()
  tourney_location = result %>% html_nodes(css = ".tourney-location") %>% html_text() %>% str_trim()
  tourney_date = result %>% html_nodes(css = ".tourney-dates") %>% html_text() %>% str_trim() %>% ymd()

  surface =
    result %>% html_nodes(css = "span.item-value") %>%
    html_text() %>%
    str_trim() %>% .[3]

  url =
    result %>%
    html_nodes(css = ".button-border") %>%
    html_attr(name = "href") %>%
    str_c("https://www.atptour.com", .)

  cbind.data.frame(tourney_name, tourney_location, surface, tourney_date, url) %>%
    return()
}

Fetch_Page <- function(page) {
  print(page)
  HTML = page %>% read_html()
  Results = HTML %>% html_nodes(css = ".tourney-result")

  Results %>% map_df(Fetch_result) %>%
    return()
}

FETCH <- function(url) {
  require(rvest)
  print(url)
  HTML = url %>% read_html()
  toto = HTML %>% html_table(fill = T)

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

  Results %>% distinct(round) %>% rowid_to_column() %>%
    mutate(rowid=dense_rank(desc(rowid))) %>%
    inner_join(x=Results, by="round") %>%
    return()
}

REKORD <- function(url) {
  require(jsonlite)
  url %>% FETCH() %>%
    group_by(round, rowid) %>% nest() %>%
    jsonlite::write_json(path = str_c("data/",
                                      str_remove_all(string = url,
                                                     pattern = "[^[:alnum:]]"),
                                      ".json"))
}

UNJSON <- function(file) {
  require(jsonlite)
  require(purrr)
  file %>%
    fromJSON(flatten = T) %>%
    unnest %>%
    return()

}

GLICKO <- function(input) {
  require(PlayerRatings)

  output =
    input %>%
    mutate(Date = ymd(Date)) %>%
    mutate(Time =
             min(Date) %--% Date %>%
             as.period() %>% day()) %>%
    select(Time, winner_name, loser_name) %>%
    mutate(Score = 1)

  Elo =
    output %>%
    mutate_if(.predicate = is.factor, .funs = as.character) %>%
    glicko2(gamma = 0, tau = 1.2)

  return(Elo)
}
