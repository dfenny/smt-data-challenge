data_directory = '../data/'
library(ggplot2)
library(arrow)
library(dplyr)
library(patchwork)
game_info <- arrow::open_csv_dataset(paste0(data_directory, "game_info"), 
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))
game_events <- arrow::open_csv_dataset(paste0(data_directory,"game_events"), 
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))
ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"ball_pos"), 
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_info_ball <- game_info |>
  filter(inning > 0, top_bottom == "bottom", !is.na(batter)) |>
  collect() |>
  select(game_str, play_per_game, top_bottom, batter)

game_events_ball <- game_events|>
  filter() |>
  collect() |>
  select(game_str, play_id, play_per_game, event_code, timestamp)

ball_pos <- ball_pos |>
  filter() |>
  collect() |>
  select(game_str, play_id, timestamp, ball_position_x, ball_position_y, ball_position_z)

ball_pos_hit <- merge(x = ball_pos, y = game_events_ball, by = c("game_str", "play_id", "timestamp"))
   
ball_id <- merge(x = ball_pos_hit, y = game_info_ball, by = c("game_str", "play_per_game"))

ball_id |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

hits_location <- ball_id |>
  arrange(batter,game_str, play_id, timestamp) |>
  filter(((lag(event_code) == 4 & play_id == lag(play_id)))) |>
  ungroup()

fair_hits <- hits_location |>
  group_by(game_str, play_id) |>
  #filter(n() == 2) |>
  ungroup()

fair_hits |> ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
fair_hits$ball_dist <- 0
for(i in 1:nrow(fair_hits)) {
  cur_row <- fair_hits[i,]
  cur_row$ball_dist <- sqrt((cur_row$ball_position_x^2) + (cur_row$ball_position_y^2))
  fair_hits[i,] <- cur_row
}
fair_hits |>
  filter(batter == 334) |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

batter_hits <- merge(x = fair_hits, y = batter_id, by = c("game_str", "play_id", "play_per_game")) |>
  select(game_str, play_id, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter.x, ball_dist, field_x, field_y, hand) |>
  filter(batter.x < 1000)

names(batter_hits)[names(batter_hits) == 'batter.x'] <- 'batter'

plot1 <- batter_hits |>
  filter(batter == 993, hand == 'R') |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 <- batter_hits |>
  filter(batter == 993, hand == 'L') |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 + plot1

batter_hits$hit_side <- 'R'
batter_hits$push_pull <- 'push'
for(i in 1:nrow(batter_hits)) {
  cur_row <- batter_hits[i,]
  if(cur_row$ball_position_x < 0) {
    cur_row$hit_side <- 'L'
  }
  if(cur_row$hit_side != cur_row$hand) {
    cur_row$push_pull <- 'pull'
  }
  batter_hits[i,] <- cur_row
}
write.csv(batter_hits, "combined_batter_hits.csv")
grouped_hits <- batter_hits |>
  group_by(batter,hand) |>
  summarize(
    pull <- sum(push_pull == "pull"),
    push <- sum(push_pull == "push")
  ) 
names(grouped_hits) <- c("battter", "hand", "pull", "push")
write.csv(grouped_hits, "hit_outcomes.csv")
