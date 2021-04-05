library(bigchess)
library(dplyr)
library(stringr)
library(lubridate)

candidates <- c(
  "Nepo", "MVL", "Caruana", "Ding Liren", "Wang Hao",
  "Giri", "Grischuk", "Alekseenko"
)

read_pgn <- function(filepath){
  read.pgn(
    filepath,
    add.tags=c("WhiteElo", "BlackElo", "ECO"),
    extract.moves=0,
    stat.moves=FALSE
  ) %>%
    select(-Site, -Round, -Movetext, -NMoves) %>%
    filter(!is.na(ECO)) %>%  # Fischer random
    mutate(
      Date=as_date(Date),
      across(
        c(White, Black),
        ~case_when(
          !grepl(",", .x)                             ~ .x,
          grepl("Vachier", .x) & grepl("Lagrave", .x) ~ "MVL",
          grepl("Ding", .x) & grepl("Liren", .x)      ~ "Ding Liren",
          grepl("Wang", .x) & grepl("Hao", .x)        ~ "Wang Hao",
          grepl("Nepomniachtchi", .x)                 ~ "Nepo",
          TRUE                                        ~ str_extract(.x, ".*(?=,)")
        )
      )
    )
}
