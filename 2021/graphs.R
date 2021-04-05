library(ggplot2)
library(ggthemr)
library(ggrepel)
library(dplyr)

ggthemr("solarized")
source("2021/common.R")

# Rating evolution for selected players
p <- candidates
draws %>%
  pivot_longer(all_of(p), names_to="player", values_to="rating") %>%
  filter(player %in% p) %>%
  group_by(date, player) %>%
  summarise(rating=mean(rating), .groups="drop") %>% {
    ggplot(., aes(x=date, y=rating, group=player, color=player)) +
      geom_line(size=1) +
      geom_text_repel(
        data=filter(., date==max(.$date)),
        mapping=aes(label=player, x=as_date("2021-04-19")),
        size=3,
        direction="y"
      ) +
      scale_x_date(
        expand=c(0,0), date_labels="%b %Y",
        sec.axis=dup_axis(labels=NULL, breaks=NULL)
      ) +
      scale_y_continuous(sec.axis=dup_axis(name=NULL), expand=c(0,0)) +
      scale_color_brewer(type="qual", palette=2) +
      scale_fill_brewer(type="qual") +
      annotate("text", label="kwiatkowski.io", x=as_date("2019-01-20"), y=0.0087, size=2.5) +
      labs(
        x=NULL,
        y="Rating",
        title="The Candidates",
        subtitle="Ratio of ratings is ratio of win probabilities in a single game"
      ) +
      theme(legend.position="none") +
      coord_cartesian(ylim=c(0.008, 0.045))
  }

# Spaghetti plot of a player's rating
p <- "Carlsen"
samples <- 500
draws %>%
  mutate(rating=.data[[p]]) %>%
  select(date, .draw, rating) %>%
  group_by(date) %>%
  mutate(type="draw") %>%
  do(bind_rows(., as_tibble(list(date=.$date, .draw=-1, rating=mean(.$rating), type="mean")))) %>%
  ungroup() %>%
  filter(type == "mean" | .draw %in% sample(length(unique(.$.draw)), samples)) %>% {
    ggplot(., aes(x=date, y=rating, group=.draw)) +
      geom_line(data=filter(., type=="draw"), alpha=0.1, size=0.5, color="#268bd2") +
      geom_line(data=filter(., type=="mean"), alpha=0.5, size=0.5, color="#dc322f") +
      scale_x_date(date_labels="%b %Y", expand=c(0,0), sec.axis=dup_axis(breaks=NULL)) +
      scale_y_continuous(sec.axis=dup_axis(name=NULL), expand=c(0,0)) +
      annotate("text", label="kwiatkowski.io", x=as_date("2019-01-20"), y=0.0175, size=2.5) +
      labs(
        x=NULL,
        y="Rating",
        title="Magnus Carlsen",
        subtitle=NULL
      ) +
      coord_cartesian(ylim=c(0.015, 0.17))
  }


# Spaghetti plot of win probability against a reference player
p <- "Caruana"
ref <- "Carlsen"
samples <- 500
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
  ungroup() %>%
  filter(type == "mean" | .draw %in% sample(length(unique(.$.draw)), samples)) %>% {
  ggplot(., aes(x=date, y=win2, group=.draw)) +
  geom_line(data=filter(., type=="draw"), alpha=0.1, size=0.5, color="#268bd2") +
  geom_line(data=filter(., type=="mean"), alpha=0.5, size=0.5, color="#dc322f") +
  scale_x_date(date_labels="%b %Y", expand=c(0,0), sec.axis=dup_axis(breaks=NULL)) +
  scale_y_continuous(
    breaks=seq(0, 1, 0.2),
    labels=c("0%", "20%", "40%", "60%", "80%", "100%"),
    sec.axis=dup_axis(name=NULL),
    expand=c(0,0)
  ) +
  annotate("text", label="kwiatkowski.io", x=as_date("2019-01-20"), y=0.16, size=2.5) +
  theme(legend.position="none") +
  labs(
    x=NULL,
    y=sprintf("Probability of %s win", p),
    title=sprintf("%s vs %s, no draws allowed", p, ref),
    subtitle=sprintf("(Average probability of draw: %.1f%%)", 100*mean(na.omit(.$draw)))
  ) +
  coord_cartesian(ylim=c(0.15, 0.65))
}

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
  arrange(desc(ratingN)) %>%
  mutate(CWR=100.*ratingN/(ratingN+0.0679)) %>%
  print(n=100)

# Signal variances
fit$summary("r") %>%
  mutate(player=players[as.integer(str_extract(variable, "[0-9]+"))]) %>%
  arrange(mean) %>%
  print(n=100)

