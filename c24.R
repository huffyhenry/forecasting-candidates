
K <- length(tbls)

# Points for games won/drawn/lost
reduce(tbls, bind_rows) %>%
mutate(
  win_pts=case_when(
    player == "Caruana" ~ 2*wins,
    player == "Nepomniachtchi" ~ 3*wins,
    player == "Wang Hao" ~ 5*wins,
    player == "Giri"     ~ 5*wins,
    player == "Ding Liren" ~ 2*wins,
    player == "Grischuk" ~ 4*wins,
    player == "MVL" ~ 4*wins,
    player == "Alekseenko" ~ 6*wins
  ),
  loss_pts=case_when(
    player == "Caruana" ~ 8*(14 - wins - draws),
    player == "Nepomniachtchi" ~ 4*(14 - wins - draws),
    player == "Wang Hao" ~ 4*(14 - wins - draws),
    player == "Giri"     ~ 5*(14 - wins - draws),
    player == "Ding Liren" ~ 8*(14 - wins - draws),
    player == "Grischuk" ~ 4*(14 - wins - draws),
    player == "MVL" ~ 4*(14 - wins - draws),
    player == "Alekseenko" ~ 3*(14 - wins - draws)
  ),
  draw_pts=ifelse(player=="Giri", 0.5, 1)*draws
) %>%
group_by(player) %>%
summarise(
  wins_xp=mean(win_pts),
  loss_xp=mean(loss_pts),
  draw_xp=mean(draw_pts)
) %>%
arrange(desc(wins_xp))

# First-place tie on points
soft.tied <- function(tbl){
  filter(tbl, points==max(points)) %>% nrow() > 1
}
2-2*(sum(as.numeric(map(tbls, soft.tied))) + 66)/10000

# Grischuk-Nepo head to head
gn.hth <- function(tbl){
  t <- tbl %>%
    filter(player %in% c("Grischuk", "Nepomniachtchi")) %>%
    arrange(player) %>%
    pull(points)

  if (t[1] == t[2])
    return("draw")
  else if (t[1] > t[2])
    return("Grischuk")
  else
    return("Nepo")
}
gn <- map(tbls, gn.hth)
6*length(gn[gn=="draw"])/length(gn)
4*length(gn[gn=="Grischuk"])/length(gn)
4*length(gn[gn=="Nepo"])/length(gn)

# MVL's/Giri's final score
player_name <- "Giri"
reduce(tbls, bind_rows) %>%
filter(player == player_name) %>%
mutate(
  score=case_when(
    points >= 7.5 ~ "+1 or better",
    points <= 6.5 ~ "-1 or worse",
    points == 7   ~ "even"
  )
) %>%
group_by(score) %>%
summarise(
  prop=n()/nrow(.)
)

# Draws in Caruana-Ding
cd.draws <- function(sim){
  sim %>%
  filter(white %in% c("Caruana", "Ding Liren") & black %in% c("Caruana", "Ding Liren")) %>%
  filter(result == "1/2-1/2") %>%
  nrow() == 2
}
7*(length(sims) - sum(as.numeric(map(sims, cd.draws))))/length(sims)

# Alekseenko last/tied for last?
a.last <- function(tbl){
  tbl %>% filter(player == "Alekseenko" & points == min(points)) %>% nrow() > 0
}
3*sum(as.numeric(map(tbls, a.last)))/length(tbls)

# Number of decisive games
decisive <- function(sim){
  sim %>% filter(result != "1/2-1/2") %>% nrow()
}
d <- as.numeric(map(sims, decisive))
hist(d)
mean(d)
names(sort(-table(d)))[1] # Seriously, R.
