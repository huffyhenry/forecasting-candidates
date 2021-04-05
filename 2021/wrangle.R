library(dplyr)
library(tidyr)
library(purrr)
library(readr)

source("common.R")

all.games <- list.files("data/pgn/", full.names=TRUE) %>%
  lapply(read_pgn) %>%
  reduce(bind_rows) %>%
  arrange(desc(Date), Event)

top.players <- all.games %>%
  pivot_longer(c(White, Black), names_to="Color", values_to="Player") %>%
  mutate(Elo=ifelse(Color=="White", WhiteElo, BlackElo)) %>%
  select(Player, Elo) %>%
  group_by(Player) %>%
  summarise(MinElo=min(Elo), MaxElo=max(Elo)) %>%
  filter(MaxElo > 2700 & Player != "Kasparov") %>%
  pull(Player)

event.speed <- all.games %>%
  select(Event, Date, White, Black) %>%
  pivot_longer(c(White, Black), values_to="Player") %>%
  group_by(Event, Player, Date) %>%
  summarise(Games=n(), .groups="drop") %>%
  group_by(Event, Date) %>%
  summarise(DaySpeed=max(Games), .groups="drop")

dataset <- all.games %>%
  filter(White %in% top.players & Black %in% top.players) %>%
  filter(!grepl("[B|b]litz", Event)) %>%
  filter(!grepl("[S|s]peed", Event)) %>%
  filter(!grepl("[B|b]ullet", Event)) %>%
  filter(!grepl("[A|a]rmageddon", Event)) %>%
  filter(!grepl("Clutch", Event)) %>%
  filter(!grepl("Titled Tue", Event)) %>%
  filter(!grepl("Chess24 Banter", Event)) %>%
  filter(!grepl("Pepe Cuenca", Event)) %>%
  filter(!grepl("MrDodgy", Event)) %>%
  filter(!grepl("Chessbrah", Event)) %>%
  filter(!grepl("Online Chess Stars Final", Event)) %>%
  filter(!grepl("Yuri Razuvaev", Event)) %>%
  left_join(event.speed) %>%
  mutate(
    Result=as.character(Result),
    SpeedChess=case_when(
      DaySpeed > 2                   ~ 1,
      grepl("[R|r]apid", Event)      ~ 1,
      grepl("Online Olym", Event)    ~ 1,
      grepl("Online Nations", Event) ~ 1,
      TRUE                           ~ 0
    )
  ) %>%
  select(-DaySpeed) %>%
  write_csv("data/results.csv")


# carlsen.tour <- c(
#   "Carlsen Inv Prelim",
#   "Carlsen Inv Final 4",
#   "Lindores Abbey Prelim",
#   "Lindores Abbey Final 8",
#   "Chessable Masters GpA",
#   "Chessable Masters GpB",
#   "Chessable Masters Final 8",
#   "Legends of Chess Prelim",
#   "Legends of Chess Final",
#   "Carlsen Tour Final 2020",
#   "Skilling Open Prelim",
#   "Skilling Open KO 2020",
#   "Airthings Masters Prelim",
#   "Airthings Masters KO 2020",
#   "Opera Euro Rapid Prelim",
#   "Opera Euro Rapid KO 2021",
#   "Magnus Carlsen Inv Prelim",
#   "Magnus Carlsen Inv KO"
# )
