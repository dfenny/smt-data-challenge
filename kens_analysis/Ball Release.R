data_directory = ''
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
   
ballID <- merge(x = ball_pos_hit, y = game_info_ball, by = c("game_str", "play_per_game"))

ballID |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

hitsLocation <- ballID |>
  arrange(batter,game_str, play_id, timestamp) |>
  filter(((lag(event_code) == 4 & play_id == lag(play_id)))) |>
  ungroup()

fairHits <- hitsLocation |>
  group_by(game_str, play_id) |>
  #filter(n() == 2) |>
  ungroup()

fairHits |> ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
fairHits$ballDist <- 0
for(i in 1:nrow(fairHits)) {
  curRow <- fairHits[i,]
  curDist <- sqrt((curRow$ball_position_x^2) + (curRow$ball_position_y^2))
  curRow$ballDist <- curDist
  fairHits[i,] <- curRow
}
fairHits |>
  filter(batter == 334) |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

batter_hits <- merge(x = fairHits, y = batterID, by = c("game_str", "play_id", "play_per_game")) |>
  select(game_str, play_id, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter.x, ballDist, field_x, field_y, hand) 

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
  curRow <- batter_hits[i,]
  if(curRow$ball_position_x < 0) {
    curRow$hit_side <- 'L'
  }
  if(curRow$hit_side != curRow$hand) {
    curRow$push_pull <- 'pull'
  }
  batter_hits[i,] <- curRow
}

groupedHits <- batter_hits |>
  group_by(batter,hand) |>
  summarize(
    pull <- sum(push_pull == "pull"),
    push <- sum(push_pull == "push")
  ) |>
  filter(batter < 1000)
names(groupedHits) <- c("battter", "hand", "pull", "push")

