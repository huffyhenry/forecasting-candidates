library(stringr)
library(ggplot2)
library(ggthemr)
library(ggrepel)

ggthemr("solarized")

# Ratings against Elo
ratings %>%
filter(games > 50 | player == "Alekseenko") %>%
ggplot(aes(x=avg_elo, y=rating)) +
geom_text_repel(aes(label=player), segment.color="gray", size=3) +
geom_point(aes(color=candidate)) +
labs(
  x="Average Elo from 30 Nov 2016",
  y="Davidson rating"
) +
annotate('text', x=2850, y=0.0, label="@statlurker", size=3, hjust=0) +
theme(legend.position="none", text=element_text(family="Palatino"))

# Expected points vs given player
player_name <- "Svidler"
plot_limits <- c(0.35, 0.68)
player_idx <- match(player_name, players)
player_elo <- filter(ratings, player==player_name) %>% pull(avg_elo)

ratings %>%
filter(games > 50 | player == "Alekseenko") %>%
filter(player != player_name) %>%
mutate(xp_elo=1/(1 + 10^((player_elo - avg_elo)/400))) %>%
left_join(
  data.frame(
    xp=apply(-0.5*fit$hth[,,player_idx] + 1.5, 2, mean),
    player=players,
    stringsAsFactors=FALSE
  )
) %>%
ggplot(aes(x=xp_elo, y=xp)) +
geom_abline(aes(slope=1, intercept=0), alpha=0.5, linetype=2) +
geom_text_repel(aes(label=player), size=2.5, segment.color="gray") +
geom_point(aes(color=candidate)) +
labs(
  title=sprintf("Expected points vs %s", player_name),
  x="Based on average Elo",
  y="Based on the model"
) +
annotate('text', x=plot_limits[[2]], y=plot_limits[[1]]+0.01,
         label="@statlurker", size=2, hjust=1) +
scale_x_continuous(expand=c(0,0)) +
scale_y_continuous(expand=c(0,0)) +
coord_cartesian(xlim=plot_limits, ylim=plot_limits) +
theme(legend.position="none", text=element_text(family="Palatino"))

# Draw probabilities per opening
data.frame(
  nu=apply(fit$eco, 2, mean),
  opening=openings,
  stringsAsFactors=FALSE
) %>%
full_join(
  expand.grid(k=seq(1, 5, 0.05), opening=openings)
) %>%
mutate(
  draw=nu*sqrt(k)/(k + 1 + nu*sqrt(k)),
  favourite=k*(1.0-draw)/(k+1)
) %>%
mutate(opening_long=case_when(
  #opening == "A00" ~ "Irregular (A00)",
  #opening == "E04" ~ "Open Catalan (E04)",
  #opening == "C88" ~ "Closed Ruy Lopez (C88)",
  #opening == "C50" ~ "King's Pawn Game (C50)"
  #opening == "A05" ~ "Reti (A05)",
  opening == "C42" ~ "Petrov (C42)",
  opening == "C43" ~ "3. d4 Petrov (C43)",
  opening == "E32" ~ "Nimzo-Indian (E32)",
  opening == "C65" ~ "Berlin (C65)",
  opening == "B90" ~ "Najdorf (B90)",
  opening == "C53" ~ "Giuocco Piano (C53)",
  opening == "C02" ~ "Advance French (C02)",
  opening == "D37" ~ "QGD, 4. Nf3 (D37)",
  opening == "D15" ~ "QGD, Slav (D15)",
  TRUE             ~ NA_character_
  ),
  highlight=!is.na(opening_long),
  note=ifelse(k > 4.95, opening_long, NA_character_)
) %>%
arrange(desc(highlight)) %>%
ggplot(aes(x=k, y=draw)) +
geom_line(aes(group=opening, color=highlight, alpha=highlight)) +
scale_alpha_manual(values=c(0.25, 1.0)) +
geom_text(aes(x=5.05, label=note), size=2.5, hjust=0) +
scale_x_continuous(expand=c(0,0)) +
scale_y_continuous(labels=scales::percent, expand=c(0,0)) +
annotate('text', x=1.05, y=0.36, label="@statlurker", size=2, hjust=0) +
labs(
  x="Ratio of player ratings",
  y="",
  title="Probability of draw"
) +
coord_cartesian(xlim=c(1.0, 5.95), ylim=c(0.35, 0.8)) +
theme(legend.position="none", text=element_text(family="Palatino"))
