library(ggplot2)
library(arrow)
library(dplyr)
library(patchwork)

batter_pitch_info <- read.csv("combined_batter_hits.csv") |>
  select(game_str, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter, ball_dist, hand, hit_side, push_pull)
pitcher_pitch_info <- read.csv("../dougs_analysis/pitcher-location.csv") |>
  select(game_str, play_per_game, x_ball_pos, pitcher_is_righty)

pitcher_vs_batter <- merge(x = batter_pitch_info, y = pitcher_pitch_info, by = c("game_str", "play_per_game"))

grouped_pitcher_vs_batter <- pitcher_vs_batter |>
  group_by(batter, pitcher_is_righty, hand) |>
  summarise(
    count = n(),
    push = sum(push_pull == "push"),
    pull = sum(push_pull == "pull"),
    dist = mean(ball_dist),
    push_mean = mean(ball_dist[push_pull == "push"]),
    pull_mean = mean(ball_dist[push_pull == "pull"])
  ) |>
  ungroup()

plot1 <- pitcher_vs_batter |>
  filter(hand == "L", pitcher_is_righty == "False") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 <- pitcher_vs_batter |>
  filter(hand == "R", pitcher_is_righty == "False") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot3 <- pitcher_vs_batter |>
  filter(hand == "L", pitcher_is_righty == "True") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot4 <- pitcher_vs_batter |>
  filter(hand == "R", pitcher_is_righty == "True") |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

(plot1 + plot2) / (plot3 + plot4)

## Eliminate all infield hits