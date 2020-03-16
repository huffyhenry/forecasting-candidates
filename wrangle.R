library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

list.files("data/raw/", full.names=TRUE) %>%
map(function(f){read.csv(f, stringsAsFactors=FALSE)}) %>%
reduce(bind_rows) %>%
gather(key="color", value="player", white, black) %>%
mutate(
  player=case_when(
    player == "Bortnyk O" ~ "Bortnyk",
    player == "Bu, Xiangzhi" ~ "Bu Xiangzhi",
    player == "Ding, Liren" ~ "Ding Liren",
    player == "Le, Quang Liem" ~ "Le Quang Liem",
    grepl("Li, Chao", player) ~ "Li Chao",
    player == "Lu, Shanglei" ~ "Lu Shanglei",
    player == "Ni, Hua" ~ "Ni Hua",
    grepl("Lagrave", player) ~ "MVL",
    player == "Wang, Hao" ~ "Wang Hao",
    player == "Wang, Yue" ~ "Wang Yue",
    player == "Wei, Yi" ~ "Wei Yi",
    player == "Yu, Yangyi" ~ "Yu Yangyi",
    !grepl(",", player) ~ player,
    TRUE ~ str_extract(player, ".*(?=,)")
  )
) %>%
spread(color, player) %>%
filter(format == "classical") %>%
filter(!grepl("chess.com", event)) %>%
filter(!(white %in% c("Stockfish", "AlphaZero", "Kasparov"))) %>%
filter(!(black %in% c("Stockfish", "AlphaZero", "Kasparov"))) %>%
arrange(desc(date)) %>%
select(date, white, black, result, opening, white_elo, black_elo) %>%
write.csv("data/games.csv", row.names=FALSE)
