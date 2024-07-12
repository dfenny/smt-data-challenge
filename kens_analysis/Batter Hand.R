data_directory <- ''
library(ggplot2)
library(arrow)
library(dplyr)
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

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"player_pos"), 
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

game_info_batter <- game_info |>
  filter(inning > 0, top_bottom == "bottom", !is.na(batter)) |>
  collect() |>
  select(game_str, play_per_game, inning, top_bottom, batter)

game_events_batter <- game_events|>
  filter() |>
  collect() |>
  select(game_str, play_id, play_per_game, event_code, timestamp)

batter_pos <- player_pos |>
  filter(player_position == 10) |> 
  collect() |>
  select(game_str, player_position, play_id, field_x, field_y, timestamp) 

playerGame <- merge(x = batter_pos, y = game_events_batter, by = c("game_str", "play_id", "timestamp")) |> 
  filter(event_code == 1) 

batterID <- merge(x = playerGame, y = game_info_batter, by = c("game_str", "play_per_game")) 

batterID |>
ggplot(aes(field_x,field_y))+
  geom_point()

batterID <- transform(batterID, hand = ifelse(field_x < 0, "L", "R"))

groupedBatter <- batterID |>
  group_by(batter) |>
  summarize(mean_x = mean(field_x), mean_y = mean(field_y),
            median_x = median(field_x), median_y = median(field_y),
            range_x = diff(range(field_x)), range_y = diff(range(field_y)),
            total_count = n(),
            left_count = sum(hand == "L"),
            right_count = sum(hand == "R")) |>
  filter(total_count > 5) |>
  ungroup()

batter_graph <- function(batterData, x_var, y_var) {
  batterData |>
    ggplot(aes(x_var, y_var)) + 
    geom_point() +
    geom_segment(aes(x = -5.7, y = 3.7, xend = -1.2, yend = 3.7)) +
    geom_segment(aes(x = -5.7, y = -2.3, xend = -1.2, yend = -2.3)) +
    geom_segment(aes(x = 1.2, y = 3.7, xend = 5.7, yend = 3.7)) +
    geom_segment(aes(x = 1.2, y = -2.3, xend = 5.7, yend = -2.3)) +
    geom_segment(aes(x = -5.7, y = -2.3, xend = -5.7, yend = 3.7)) +
    geom_segment(aes(x = -1.2, y = -2.3, xend = -1.2, yend = 3.7)) +
    geom_segment(aes(x = 5.7, y = -2.3, xend = 5.7, yend = 3.7)) +
    geom_segment(aes(x = 1.2, y = -2.3, xend = 1.2, yend = 3.7)) +
    geom_segment(aes(x = 0, y = 0, xend = -0.7, yend = 0.7)) +
    geom_segment(aes(x = 0, y = 0, xend = 0.7, yend = 0.7)) +
    geom_segment(aes(x = -0.7, y = 0.7, xend = -0.7, yend = 1.42)) +
    geom_segment(aes(x = 0.7, y = 0.7, xend = 0.7, yend = 1.42)) +
    geom_segment(aes(x = -0.7, y = 1.42, xend = 0.7, yend = 1.42)) 
}

batter_graph(groupedBatter, groupedBatter$mean_x, groupedBatter$mean_y)
batter_graph(batterID, batterID$field_x, batterID$field_y)




