library(dplyr)
remove(list = ls())
box <- read.csv("data/box.csv", stringsAsFactors = FALSE)
box$game_id <- sprintf("%010d", box$game_id)
path <- "games/2017/"
game_files <- list.files(path)
game_files <- game_files[grepl("002", game_files)]

bind_data <- list()

for (i in 1:length(game_files)) {
  cat("... processing game:", game_files[i], "\n")
  data_i <- jsonlite::fromJSON(paste0(path, game_files[i]))
  game_id <- gsub(".json", "", game_files[i])
  bind_data[[i]] <- data.frame(game_id = game_id, data_i, stringsAsFactors = FALSE)
}

bind_data <- do.call("rbind", bind_data)

bind_data <- bind_data %>%
  left_join(dplyr::select(box, game_id, h_abb, v_abb), by = c("game_id" = "game_id"))

bind_data$wpa_cond <- bind_data$player1_team == bind_data$h_abb
bind_data$wpa_cond <- sapply(bind_data$wpa_cond, function(x) {ifelse(x == TRUE, 1, -1)})

wip <- bind_data %>%
  filter(!is.na(player1_name), !is.na(player1_team)) %>%
  group_by(player1_id, player1_name, player1_team) %>%
  summarize(wip = sum(lev * wpa_cond)) %>%
  arrange(desc(wip))

head(data.frame(wip), n = 50)
filter(wip, player1_team == "GSW")
filter(wip, player1_team == "BKN")
filter(wip, player1_team == "CLE")
filter(wip, player1_team == "NYK")
filter(wip, player1_team == "BOS")
filter(wip, player1_team == "MIN")
filter(wip, player1_team == "MIL")
filter(wip, player1_team == "OKC")
filter(wip, player1_team == "LAL")
filter(wip, player1_team == "LAC")
filter(wip, player1_team == "UTA")
filter(wip, player1_team == "MIA")
filter(wip, player1_team == "PHI")
