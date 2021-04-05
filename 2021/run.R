library(cmdstanr)
library(posterior)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)

results <- read_csv("2021/data/results.csv") %>%
  mutate(
    ResultCode=case_when(
      Result == "1-0" ~ 1,
      Result == "0-1" ~ 3,
      TRUE            ~ 2
    )
  ) %>%
  arrange(Date, Event)

players <- unique(c(results$White, results$Black))

dates <- c(unique(results$Date), as_date('2021-04-19'))

stan_data <- list(
  n_players=length(players),
  n_games=nrow(results),
  n_dates=length(dates),
  white=match(results$White, players),
  black=match(results$Black, players),
  result=results$ResultCode,
  date_idx=match(results$Date, dates),
  speedchess=results$SpeedChess,
  dates=as.numeric(dates - min(dates))
)

model <- cmdstan_model("2021/model.stan", dir="2021/compiled-models/")
fit <- model$sample(
    data=stan_data,
    chains=4,
    parallel_chains=4,
    iter_warmup=400,
    iter_sampling=400,
    refresh=10
)
fit$cmdstan_diagnose()
fit$save_object("2021/hugefit.rds")

draws <- fit$draws(variables=c("rating", "nu[1]")) %>%
  as_draws_df() %>%
  rename(nu=`nu[1]`) %>%
  pivot_longer(starts_with("rating"), names_to="variable", values_to="value") %>%
  mutate(coords=str_extract(variable, "[0-9]+,[0-9]+")) %>%
  separate(coords, c("player_idx", "date_idx"), convert=TRUE) %>%
  mutate(
    player=players[player_idx],
    date=dates[date_idx]
  ) %>%
  pivot_wider(id_cols=c(.draw, date, nu), names_from=player, values_from=value)

ratings <- fit$summary("rating") %>%
  mutate(coords=str_extract(variable, "[0-9]+,[0-9]+")) %>%
  separate(coords, c("player_idx", "date_idx"), convert=TRUE) %>%
  mutate(
    player=players[player_idx],
    date=dates[date_idx]
  ) %>%
  select(player, date, mean, median, sd, q5, q95) %>%
  group_by(date) %>%
  mutate(ref=exp(mean(log(`mean`)))) %>%
  ungroup() %>%
  left_join( # Add a marker for days with game played
    results %>%
    pivot_longer(c("White", "Black"), values_to="Player") %>%
    transmute(player=Player, date=Date, game=1) %>%
    distinct()
  )
