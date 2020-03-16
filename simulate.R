# Assumes that we have the lists `players` and `candidates`,
# and the `fit` object from run.R
library(purrr)

# Simulate the tournment once
sim.once <- function(sim.id){
  result.code <- c("1-0", "1/2-1/2", "0-1")
  white <- c()
  black <- c()
  result <- c()

  for (c1 in candidates){
    for (c2 in candidates){
      if (c1 != c2){
        white <- c(white, players[c1])
        black <- c(black, players[c2])
        result <- c(result, result.code[sample(fit$hth[, c1, c2], 1)])
      }
    }
  }

  data.frame(
    white=white,
    black=black,
    result=result,
    sim=sim.id,
    stringsAsFactors=FALSE
  )
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
    draws=sum(draw)
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
    summarise(sb=sum(sb_contribution))
  )

  # Add the tiebreakers to the table and sort
  suppressMessages(
    tbl %>%
    left_join(hth) %>%
    left_join(sb) %>%
    group_by(points, hth_pts, wins, sb) %>%
    mutate(playoff=as.numeric(n() > 1)) %>%
    ungroup() %>%
    arrange(desc(points), desc(hth_pts), desc(wins), desc(sb)) %>%
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


