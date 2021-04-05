# Lightly adapted from the 2020 project.
# Assumes that we have the `draws` object from run.R.
library(purrr)

source("common.R")

f <- function(x, t){
  filter(t, player==x)$points
}

first.leg <- read_pgn("data/pgn/.candidates2020.pgn") %>%
  select(White, Black, Result) %>%
  rename_with(tolower)

hth <- draws %>%
  filter(date == "2021-04-19") %>%
  select(one_of(c("nu", candidates))) %>%
  mutate(.draw=row_number()) %>%
  pivot_longer(all_of(candidates), names_to="white", values_to="w") %>%
  full_join(x=., y=transmute(., black=white, b=w, .draw=.draw), by=".draw") %>%
  filter(white != black) %>%
  anti_join(first.leg) %>%
  mutate(
    pwhite=w/(w + b + nu*sqrt(w*b)),
    pblack=b/(w + b + nu*sqrt(w*b)),
    pdraw=1 - pwhite - pblack
  ) %>%
  select(white, black, pwhite, pblack, pdraw, .draw, w, b)

n.draws <- length(unique(hth$.draw))

# Simulate the tournament once
sim.once <- function(sim.id){
  result.code <- c("1-0", "1/2-1/2", "0-1")
  draw.id <- sample(n.draws, 1)

  hth %>%
    filter(.draw==draw.id) %>%
    rowwise() %>%
    mutate(result=result.code[sample(3, 1, prob=c(pwhite, pdraw, pblack))]) %>%
    select(white, black, result) %>%
    bind_rows(first.leg) %>%
    mutate(sim.id=sim.id)
}

# Create a basic league table from a simulation
init.table <- function(sim){
  sim %>%
  gather(key="color", value="player", white, black) %>%
  mutate(
    win=(result=="1-0" & color == "white") | (result=="0-1" & color == "black"),
    draw=(result == "1/2-1/2")
  ) %>%
  mutate_at(c("win", "draw"), as.numeric) %>%
  group_by(player) %>%
  summarise(
    points=sum(win + 0.5*draw),
    wins=sum(win),
    draws=sum(draw),
    .groups="drop"
  ) %>%
  arrange(desc(points))
}

# Transform a simulation into a full tournament table with tie-breakers.
# Per Wikipedia, the tie-breakers are: head-to-head score, number of wins,
# Sonneborn-Berger and a rapid playoff (not included here).
make.table <- function(sim){
  tbl <- init.table(sim)

  # Head-to-head score.
  # Calculated by making mini-tables for groups of players tied on points.
  hth <- tbl %>%
    group_by(points) %>%
    do(
      filter(sim, (white %in% .$player) & (black %in% .$player)) %>%
      init.table() %>%
      rename(hth_pts=points)
    ) %>%
    ungroup() %>%
    select(player, hth_pts)

  # Sonneborn-Berger.
  # Calculated by extending each game with white and black total points,
  # then transforming these into S-B contributions based on game result,
  # then aggregating by player.
  sb <- suppressMessages(
    sim %>%
    left_join(transmute(tbl, white=player, white_pts=points)) %>%
    left_join(transmute(tbl, black=player, black_pts=points)) %>%
    gather(key="color", value="player", white, black) %>%
    mutate(
      sb_contribution=case_when(
        color == "white" & result == "1-0"     ~ black_pts,
        color == "black" & result == "0-1"     ~ white_pts,
        color == "white" & result == "1/2-1/2" ~ 0.5*black_pts,
        color == "black" & result == "1/2-1/2" ~ 0.5*white_pts,
        TRUE                                   ~ 0
      )
    ) %>%
    group_by(player) %>%
    summarise(sb=sum(sb_contribution), .groups="drop")
  )

  # Add the tiebreakers to the table and sort
  suppressMessages(
    tbl %>%
    left_join(hth) %>%
    left_join(sb) %>%
    group_by(points, hth_pts, wins, sb) %>%
    mutate(playoff=as.numeric(n() > 1)) %>%
    ungroup() %>%
    arrange(desc(points), desc(wins), desc(sb)) %>%  #desc(hth_pts)
    mutate(rank=row_number())
  )
}

# Is there a tie for the first (or `pos`) place?
tied <- function(tbl, pos=1){
  tbl %>% filter(rank == pos && playoff == 1) %>% nrow() > 0
}

# Simulate the tournament repeatedly
N <- 10000
sims <- list()
for (k in 1:N){
  sims[[k]] <- sim.once(k)
}

# Analyse the simulations.
# Making 10'000 tables takes about 15 minutes.
tbls <- discard(map(sims, make.table), tied)
print(sprintf("%d sim(s) discarded due to tie for first place.", N-length(tbls)))
K <- length(tbls)

print("Win probabilities:")
reduce(tbls, bind_rows) %>%
filter(rank == 1) %>%
group_by(player) %>%
summarise(n=n(), prob=n()/K, odds=1/prob, nice_prob=sprintf("%.1f%%", 100*prob)) %>%
arrange(desc(n))


