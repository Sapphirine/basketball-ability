library(NBAapi)
library(dplyr)

remove(list=ls())

team_info <- get_teaminfo(LeagueID = "00") %>%
  dplyr::select(abb = ABBREVIATION, team_id = TEAM_ID) %>%
  arrange(abb)
team_info <- team_info[1:30,]
team_info$stan_id <- 1:30

game_id <- paste0("00216", sprintf("%05d", 595:1230))

out_box <- list()
out_pbp <- list()

for (i in 1:length(game_id)) {
  Sys.sleep(2)
  game_num <- game_id[i]
  cat("... downloading game:", game_num, "\n")
  
  game_box <- get_boxscoresummaryv2(GameID = game_num)[[1]] %>%
    dplyr::select(h_id = HOME_TEAM_ID, v_id = VISITOR_TEAM_ID) %>%
    mutate(game_id = game_num)
  Sys.sleep(0.5)
  game_pbp <- get_pbp2(GameID = game_num, StartPeriod = "0", EndPeriod = "10") %>%
    mutate(game_id = game_num)
  
  out_box[[i]] <- game_box
  out_pbp[[i]] <- game_pbp  
}

out_box <- do.call("rbind", out_box)
out_pbp <- do.call("rbind", out_pbp)

# out_box <- as.data.frame(t(out_box), stringsAsFactors = FALSE)
# out_box$h_id <- as.numeric(out_box$h_id)
# out_box$v_id <- as.numeric(out_box$v_id)

out_box <- left_join(out_box, dplyr::select(team_info, abb, team_id), by = c("h_id" = "team_id")) %>%
  dplyr::select(h_id, h_abb = abb, v_id, game_id)
out_box <- left_join(out_box, dplyr::select(team_info, abb, team_id), by = c("v_id" = "team_id")) %>%
  dplyr::select(h_id, h_abb, v_id, v_abb = abb, game_id)

write.csv(out_box, "data/box_2016_1.csv", row.names = FALSE)
write.csv(out_pbp, "data/pbp_2016_1.csv", row.names = FALSE)
