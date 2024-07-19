library(ggplot2)
library(arrow)
library(dplyr)
library(patchwork)

batter_pitch_info <- read.csv("combined_batter_hits.csv") |>
  select(game_str, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter, ball_dist, hand, hit_side, push_pull)
pitcher_pitch_info <- read.csv("../dougs_analysis/pitcher-location.csv") |>
  select(game_str, play_per_game, x_ball_pos)

pitcher_pitch_info$pitcher_hand = 'R'
for(i in 1:nrow(pitcher_pitch_info)) {
  cur_row <- pitcher_pitch_info[i,]
  if(is.na(cur_row$x_ball_pos)) {
    cur_row$pitcher_hand <- NA
  }
  else if(cur_row$x_ball_pos > 0) {
    cur_row$pitcher_hand = 'L'
  }
  pitcher_pitch_info[i,] <- cur_row
}

pitcher_vs_batter <- merge(x = batter_pitch_info, y = pitcher_pitch_info, by = c("game_str", "play_per_game"))

grouped_pitcher_vs_batter <- pitcher_vs_batter |>
  group_by(batter, pitcher_hand, hand) |>
  summarise(
    count = n(),
    push = sum(push_pull == "push"),
    pull = sum(push_pull == "pull")
  ) |>
  ungroup()

plot1 <- pitcher_vs_batter |>
  filter(batter == 786, hand == "L", pitcher_hand == "R") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 <- pitcher_vs_batter |>
  filter(batter == 786, hand == "R", pitcher_hand == "R") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot3 <- pitcher_vs_batter |>
  filter(batter == 786, hand == "L", pitcher_hand == "L") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot4 <- pitcher_vs_batter |>
  filter(batter == 786, hand == "R", pitcher_hand == "L") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

(plot1 + plot2) / (plot3 + plot4)