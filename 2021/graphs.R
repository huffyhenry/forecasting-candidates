library(ggplot2)
library(ggthemr)
library(ggrepel)
library(dplyr)

ggthemr("solarized")
source("2021/common.R")

# Rating evolution for selected players
p <- candidates
draws %>%
  rowwise()
  mutate()
  pivot_longer(all_of(p), names_to="player", values_to="rating") %>%
  filter(player %in% p) %>%
  group_by(date, player) %>%
  summarise(
    q5=quantile(rating, 0.05),
    q95=quantile(rating, 0.95),
    rating=mean(rating)
  ) %>% {
    ggplot(., aes(x=date, y=rating, group=player, color=player)) +
    geom_vline(aes(xintercept=as_date("2020-03-26")), color="gray", size=0.5) +
#    geom_ribbon(aes(ymin=q5, ymax=q95, fill=player), alpha=0.4) +
    geom_line(size=1) +
    geom_text_repel(
      data=filter(., date==max(.$date)),
      mapping=aes(label=player, x=as_date("2021-04-30")),
      size=3,
      direction="y"
    ) +
    scale_x_date(expand=c(0,0)) +
    scale_color_brewer(type="qual", palette=2) +
    scale_fill_brewer(type="qual") +
    labs(x=NULL, y="Rating") +
    theme(legend.position="none")
  }

# Spaghetti plot of win probability against Carlsen
p <- "Giri"
ref <- "Carlsen"
draws %>%
  mutate(
    den=(.data[[ref]] + .data[[p]] + nu*sqrt(.data[[p]]*.data[[ref]])),
    win=.data[[p]]/den,
    loss=.data[[ref]]/den,
    draw=1 - win - loss,
    win2=win/(win+loss)
  ) %>%
  select(date, .draw, draw, win2) %>%
  group_by(date) %>%
  mutate(type="draw") %>%
  do(bind_rows(., as_tibble(list(date=.$date, .draw=-1, win2=mean(.$win2), type="mean")))) %>%
  ungroup() %>% {
  ggplot(., aes(x=date, y=win2)) +
  geom_line(aes(group=.draw, alpha=type, size=type)) +
  scale_x_date(date_breaks="6 months", date_labels="%e %b %Y", expand=c(0,0)) +
  scale_y_continuous(labels=scales::percent, sec.axis=dup_axis(name=NULL)) +
  scale_alpha_manual(values=c(0.1, 1), breaks=c("draw", "mean")) +
  scale_size_manual(values=c(0.5, 1), breaks=c("draw", "mean")) +
  theme(legend.position="none") +
  labs(
    x=NULL,
    y=sprintf("Probability of %s win", p),
    title=sprintf("%s vs %s in a non-drawn game", p, ref),
    subtitle=sprintf("Average probability of draw: %.1f%%", 100*mean(na.omit(.$draw)))
  )}

# Average ratings
draws %>%
  pivot_longer(all_of(players), names_to="player", values_to="rating") %>%
  group_by(player, date) %>%
  arrange(date) %>%
  summarise(rating=mean(rating)) %>%
  summarise(
    rating_mean=mean(rating),
    rating_sd=sd(rating),
    rating0=first(rating),
    ratingN=last(rating),
    rating_delta=(ratingN-rating0)/rating0
  ) %>%
  mutate(ranking_rank=rank(rating_mean)) %>%
  left_join(
    fit$summary("tau") %>%
      transmute(
        player=players[as.integer(str_extract(variable, "[0-9]+"))],
        tau=mean
      )
  ) %>%
  arrange(desc(rating_mean)) %>%
  print(n=100)

# Signal variances
fit$summary("r") %>%
  mutate(player=players[as.integer(str_extract(variable, "[0-9]+"))]) %>%
  arrange(mean) %>%
  print(n=100)

