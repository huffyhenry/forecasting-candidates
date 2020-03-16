library(tidyr)
library(dplyr)
library(rstan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write=FALSE)  # Weird error otherwise on calls to stan()

# Load and wrangle data
data <- read.csv("games.csv", stringsAsFactors=FALSE) %>%
  filter(date > '2016-11-30')

players <- data %>%
  gather(key="color", value="player", white, black) %>%
  arrange(player) %>%
  pull(player) %>%
  unique()

candidates <- c(
  match("Ding Liren", players),
  match("Caruana", players),
  match("MVL", players),
  match("Grischuk", players),
  match("Giri", players),
  match("Nepomniachtchi", players),
  match("Wang Hao", players),
  match("Alekseenko", players)
)

openings <- data %>% arrange(opening) %>% pull(opening) %>% unique()

results <- data %>%
  mutate(
    result=case_when(
      result == "1-0" ~ 1,
      result == "1/2-1/2" ~ 2,
      result == "0-1" ~ 3
    )
  ) %>%
  pull(result)

# Prepare data for Stan and fit the model
standata <- list(
  N_players=length(players),
  N_games=nrow(data),
  N_openings=length(openings),

  white=match(data$white, players),
  black=match(data$black, players),
  opening=match(data$opening, openings),
  result=results
)

# Fit the model.
# Get a large number of samples, since it also simulates head-to-head games.
fit_raw <- rstan::stan("davidson.stan", iter=5000, data=standata)
fit <- rstan::extract(fit_raw)

# Create data summaries
ratings <- brms::predictive_interval(fit$rating, prob=0.95) %>%
  as.data.frame() %>%
  rename(rating_lwr=`2.5%`, rating_upr=`97.5%`) %>%
  mutate(player=players,  rating=apply(fit$rating, 2, mean)) %>%
  mutate(candidate=player %in% players[candidates]) %>%
  left_join(
    data %>%
    gather(key="color", value="player", white, black) %>%
    mutate(elo=ifelse(color=="white", white_elo, black_elo)) %>%
    group_by(player) %>%
    summarise(games=n(), avg_elo=mean(elo))
  ) %>%
  select(player, games, avg_elo, rating, rating_lwr, rating_upr, candidate) %>%
  arrange(desc(rating))

