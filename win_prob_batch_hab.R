.libPaths( c( .libPaths(), "/rigel/home/ima2119/rlib") )
print(.libPaths())

library(rstan)
library(dplyr)
library(zoo)

rstan_options(auto_write = TRUE)

data_box <- read.csv("box_2016.csv", stringsAsFactors = FALSE)
data_pbp <- read.csv("pbp_2016.csv", stringsAsFactors = FALSE)

game_id <- data_box$game_id

fit_model <- function(game_num, team_info, data_box, data_pbp) {
  game <- filter(data_pbp, game_id == game_num)
  load("team_priors.RData")
  
  cat("... fitting game:", game_num, "\n")
  
  home_abb <- filter(data_box, game_id == game_num)$h_abb
  away_abb <- filter(data_box, game_id == game_num)$v_abb
  
  # get latest prior
  home_team_prior <- tail(team_priors[[home_abb]], 1)
  away_team_prior <- tail(team_priors[[away_abb]], 1)
  min_prior <- min(home_team_prior, away_team_prior)
  home_effect_prior <- rep(min_prior, nrow(game))
  away_effect_prior <- rep(min_prior, nrow(game))
  home_effect_prior[1:20] <- rev(seq(min_prior, home_team_prior, length.out = 20))
  away_effect_prior[1:20] <- rev(seq(min_prior, away_team_prior, length.out = 20))
  
  # create data
  ptdiff <- sapply(game$SCOREMARGIN,
                   function(x) {ifelse(x == "TIE", 0, as.numeric(x))},
                   USE.NAMES = FALSE)
  ptdiff[1] <- 0
  score <- game$SCORE
  score[1] <- "0 - 0"
  ptdiff_ff <- na.locf(ptdiff, fromLast = FALSE)
  score <- na.locf(score, fromLast = FALSE)
  
  ptdiff_ff_norm <- (ptdiff_ff - min(ptdiff_ff)) / (max(ptdiff_ff) - min(ptdiff_ff))
  ptdiff_ff_norm <- sapply(ptdiff_ff,
                           function(x, x_min, x_max) {
                             (x - x_min)/(x_max - x_min)
                           },
                           x_min = min(ptdiff_ff), x_max = max(ptdiff_ff))
  
  simple_prior <- function(x) {
    if (x %in% c(1,3,4))
      out <- 0.02
    else if (x == 2)
      out <- 0.01
    else if (x %in% c(5, 6))
      out <- -0.02
    else
      out <- 0
    return(out)
  }
  
  home_indx <- which(game$PLAYER1_TEAM_ABBREVIATION == home_abb)
  away_indx <- which(game$PLAYER1_TEAM_ABBREVIATION == away_abb)
  
  home_prior <- rep(NA, nrow(game))
  away_prior <- rep(NA, nrow(game))
  
  home_prior[home_indx] <- sapply(game$EVENTMSGTYPE[home_indx], simple_prior)
  away_prior[away_indx] <- sapply(game$EVENTMSGTYPE[away_indx], simple_prior)
  
  home_prior[is.na(home_prior)] <- 0
  away_prior[is.na(away_prior)] <- 0
  
  # REVIST THIS!
  # Shifting the priors so that the latest event can "predict" the win prob.
  # home_prior <- c(tail(home_prior, -1), 0)
  # away_prior <- c(tail(away_prior, -1), 0)
  
  time_weight <- rep(0, nrow(game))
  last_half <- which(game$PERIOD %in% 4)
  time_weight[last_half] <- exp(seq(-3, 1, length.out = length(last_half)))
  time_weight <- time_weight * 0.1 * ptdiff_ff
  
  stan_data <- list(N = nrow(game),
                    diff = ptdiff_ff,
                    away_prior = away_prior,
                    home_prior = home_prior,
                    home_effect_prior = home_effect_prior,
                    away_effect_prior = away_effect_prior,
                    time = time_weight,
                    sigma_loc = 0,
                    sigma_scale = 2,
                    tau_loc = 0,
                    tau_scale = 2)
  
  home_abb <- filter(data_box, game_id == game_num)$h_abb
  away_abb <- filter(data_box, game_id == game_num)$v_abb
  
  fit <- stan("win_prob4.stan", data = stan_data,
              iter = 1e3, chains = 6, cores = 4,
              control = list(adapt_delta = 0.99, max_treedepth = 20),
              refresh = 1000)
  
  samples <- as.matrix(fit)
  home <- samples[,grep("home\\[", colnames(samples))]
  away <- samples[,grep("away\\[", colnames(samples))]
  
  # save the team-level prior
  home_effect <- mean(home)
  away_effect <- mean(away)
  
  team_priors[[home_abb]] <- append(team_priors[[home_abb]], home_effect)
  team_priors[[away_abb]] <- append(team_priors[[away_abb]], away_effect)
  
  # compute win probability using samples
  pbp_samples <- home - away
  probs <- sapply(1:ncol(pbp_samples),
                  function(n, dat, N) {
                    length(which(dat[,n] > 0)) / N
                  },
                  dat = pbp_samples, N = nrow(pbp_samples))
  
  data_out <- data_frame(x = 1:nrow(game), probs = probs, diff = ptdiff_ff, score = score, lev = probs - c(NA, head(probs, -1)),
                         event_num = game$EVENTNUM, event_type = game$EVENTMSGTYPE, event_action_type = game$EVENTNUM,
                         home = game$HOMEDESCRIPTION, away = game$VISITORDESCRIPTION,
                         player1_id = game$PLAYER1_ID, player1_name = game$PLAYER1_NAME, player1_team = game$PLAYER1_TEAM_ABBREVIATION,
                         player2_id = game$PLAYER2_ID, player2_name = game$PLAYER2_NAME, player2_team = game$PLAYER2_TEAM_ABBREVIATION,
                         period = game$PERIOD, game_clock = game$PCTIMESTRING, wall_clock = game$WCTIMESTRING)
  
  readr::write_lines(jsonlite::toJSON(data_out), paste0("data/2016/00",game_num, ".json"))
  
  save(team_priors, file = "team_priors.RData")
  
  pdf(paste0("figures/2016/00",game_num, ".pdf"), width = 20, height = 8)
  par(mfrow = c(1,2))
  plot(ptdiff_ff, type = "l", lwd = 2, ylim = c(-30, 30),
       main = "Point Difference (Home-Away)", xlab = "Time (Game Events)", ylab = "Point Difference")
  abline(h = 0, col = "grey", lty = 2)
  plot(probs, type = "l", lwd = 1, col = "red", ylim = c(0,1),
       main = "Win Probability", xlab = "Time (Game Events)", ylab = "Win Probability (Home)")
  abline(h = 0.5, col = "grey", lty = 2)
  mtext(bquote(bold(.(paste0(away_abb, " @ ", home_abb, " (", game_num,")")))), side = 3, line = -1.5, outer = T)
  dev.off()
}

fit_model(game_num = game_id[1], team_info = team_info, data_box = data_box, data_pbp = data_pbp)

start_time <- proc.time()
for (i in 1:3) {
  fit_model(game_num = game_id[i], team_info = team_info, data_box = data_box, data_pbp = data_pbp)
}
end_time <- proc.time()

save(start_time, end_time, file = "data/meta.RData")

