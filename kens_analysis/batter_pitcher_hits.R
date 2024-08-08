# Code 3 third file ran

library(ggplot2)
library(arrow)
library(dplyr)
library(patchwork)
library(knitr)

batter_pitch_info <- read.csv("batter_ball_hits.csv") |>
  select(game_str, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter, ball_dist, batter_hand, hit_side, push_pull)
pitcher_pitch_info <- read.csv("pitcher-location.csv") |>
  select(game_str, play_per_game, x_ball_pos, pitcher_is_righty)

batter_pitcher_hits <- merge(x = batter_pitch_info, y = pitcher_pitch_info, by = c("game_str", "play_per_game")) 
  
batter_pitcher_hits <- transform(batter_pitcher_hits, pitcher_hand = ifelse(pitcher_is_righty, "R", "L"))
  

write.csv(batter_pitcher_hits, "batter_pitcher_hits.csv")

batter_pitcher_outcomes <- batter_pitcher_hits |>
  group_by(batter, pitcher_hand, batter_hand) |>
  summarise(
    count = n(),
    push = sum(push_pull == "push"),
    pull = sum(push_pull == "pull"),
    dist = mean(ball_dist),
    push_mean = mean(ball_dist[push_pull == "push"]),
    pull_mean = mean(ball_dist[push_pull == "pull"])
  ) |>
  ungroup()



write.csv(batter_pitcher_outcomes, "batter_pitcher_outcomes.csv")

plot1 <- batter_pitcher_hits |>
  filter(batter_hand == "L", pitcher_is_righty == "False") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
  geom_point() +
  ggtitle("Left Handed Batter vs Left Handed Pitcher") +
  xlab("X Coordinate") +
  ylab("Y Coordinate") +
  theme(plot.title = element_text(hjust = 0.5))
plot2 <- batter_pitcher_hits |>
  filter(batter_hand == "R", pitcher_is_righty == "False") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
  geom_point() +
  ggtitle("Right Handed Batter vs Left Handed Pitcher") +
  xlab("X Coordinate") +
  ylab("Y Coordinate") +
  theme(plot.title = element_text(hjust = 0.5))
plot3 <- batter_pitcher_hits |>
  filter(batter_hand == "L", pitcher_is_righty == "True") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
  geom_point() +
  ggtitle("Left Handed Batter vs Right Handed Pitcher") + 
  xlab("X Coordinate") +
  ylab("Y Coordinate") +
  theme(plot.title = element_text(hjust = 0.5))
plot4 <- batter_pitcher_hits |>
  filter(batter_hand == "R", pitcher_is_righty == "True") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
  geom_point() +
  ggtitle("Right Handed Batter vs Right Handed Pitcher") + 
  xlab("X Coordinate") +
  ylab("Y Coordinate") +
  theme(plot.title = element_text(hjust = 0.5))
(plot1 + plot2) / (plot3 + plot4)

batter_ids <- unique(batter_pitcher_hits$batter)
for(id in batter_ids) {
  id = 475
  plot1 <- batter_pitcher_hits |>
    filter(batter == id, batter_hand == "L", pitcher_is_righty == "False") |>
    ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
    geom_point() +
    ggtitle("Left Handed Batter vs Left Handed Pitcher") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot2 <- batter_pitcher_hits |>
    filter(batter == id, batter_hand == "R", pitcher_is_righty == "False") |>
    ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
    geom_point() +
    ggtitle("Right Handed Batter vs Left Handed Pitcher") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot3 <- batter_pitcher_hits |>
    filter(batter == id, batter_hand == "L", pitcher_is_righty == "True") |>
    ggplot(aes(x = ball_position_x, y = ball_position_y)) +
    geom_point() +
    ggtitle("Left Handed Batter vs Right Handed Pitcher") + 
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    theme(plot.title = element_text(hjust = 0.5))
  plot4 <- batter_pitcher_hits |>
    filter(batter == id, batter_hand == "R", pitcher_is_righty == "True") |>
    ggplot(aes(x = ball_position_x, y = ball_position_y)) + 
    geom_point() +
    ggtitle("Right Handed Batter vs Right Handed Pitcher") +
    xlab("X Coordinate") +
    ylab("Y Coordinate") +
    theme(plot.title = element_text(hjust = 0.5))
  print((plot1 + plot2) / (plot3 + plot4))
}
anova_results <- data.frame(batter_id = integer(), at_bats = integer(), variable = character(), pval = double(), significant = logical())
for(id in batter_ids) {
  temp_hits <- batter_pitcher_hits |>
    filter(batter == id)
  batter_pitch_aov <- NULL
  if(nrow(temp_hits) > 2) {
    if(n_distinct(temp_hits$batter_hand) > 1) {
      batter_pitch_aov <-  aov(ball_dist ~ batter_hand * push_pull * pitcher_hand, data = temp_hits)
    }
    else if((n_distinct(temp_hits$batter_hand) == 1) & (n_distinct(temp_hits$pitcher_hand) == 1)) {
      batter_pitch_aov <-  aov(ball_dist ~  push_pull, data = temp_hits)
    }
    else {
      batter_pitch_aov <-  aov(ball_dist ~  push_pull * pitcher_hand, data = temp_hits)
    }
    aov_temp <- data.frame(summary(batter_pitch_aov)[[1]])
    colnames(aov_temp)[5] <- "PValue"
    aov_temp_min <- aov_temp[which.min(aov_temp$PValue),]
    cur_var <- row.names(aov_temp_min)
    cur_pval <- aov_temp_min$PValue
    cur_sig <- ifelse(cur_pval <= 0.05, TRUE, FALSE)
  }
  else {
    cur_pval <- NA
    cur_var <- "Not enough at bats for significant results"
    cur_sig <- NA
  }
  anova_results[nrow(anova_results) + 1,] <- c(id, nrow(temp_hits), cur_var, cur_pval, cur_sig)
}
write.csv(anova_results, "anova_results.csv")

temp_ids <- c(427,544,586,475)

for(id in temp_ids) {
  temp_hits <- batter_pitcher_hits |>
    filter(batter == id)
  batter_pitch_aov <- NULL
  if(nrow(temp_hits) > 2) {
    if(n_distinct(temp_hits$batter_hand) > 1) {
      batter_pitch_aov <-  aov(ball_dist ~ batter_hand * push_pull * pitcher_hand, data = temp_hits)
    }
    else if((n_distinct(temp_hits$batter_hand) == 1) & (n_distinct(temp_hits$pitcher_hand) == 1)) {
      batter_pitch_aov <-  aov(ball_dist ~  push_pull, data = temp_hits)
    }
    else {
      batter_pitch_aov <-  aov(ball_dist ~  push_pull * pitcher_hand, data = temp_hits)
    }
    aov_temp <- data.frame(summary(batter_pitch_aov)[[1]])
    cur_file <- paste(id, "_anova.csv")
    write.csv(aov_temp, cur_file)
    cur_data_table <- batter_pitcher_outcomes |>
      filter(batter == id)
    cur_file <- paste(id, "_data.csv")
    write.csv(cur_data_table, cur_file)
  }
}
