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

ball_hits <- hits_location |>
  group_by(game_str, play_id) |>
  #filter(n() == 2) |>
  ungroup()

ball_hits |> ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
#ball_hits$ball_dist <- 0

ball_hits <- transform(ball_hits, ball_dist = sqrt(ball_position_x^2 + ball_position_y^2))

#for(i in 1:nrow(ball_hits)) {
 # cur_row <- fair_hits[i,]
 # cur_row$ball_dist <- sqrt((cur_row$ball_position_x^2) + (cur_row$ball_position_y^2))
 # fair_hits[i,] <- cur_row
#}

ball_hits |>
  filter(batter == 334) |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()

batter_hits <- read.csv("batter_hits.csv")

batter_ball_hits <- merge(x = ball_hits, y = batter_hits, by = c("game_str", "play_id", "play_per_game")) |>
  select(game_str, play_id, play_per_game, ball_position_x, ball_position_y, ball_position_z, batter.x, ball_dist, field_x, field_y, batter_hand) |>
  filter(batter.x < 1000)

names(batter_ball_hits)[names(batter_ball_hits) == 'batter.x'] <- 'batter'

plot1 <- batter_ball_hits |>
  filter(batter == 993, batter_hand == 'R') |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 <- batter_ball_hits |>
  filter(batter == 993, batter_hand == 'L') |>
  ggplot(aes(x = ball_position_x, y = ball_position_y)) + geom_point()
plot2 + plot1

batter_ball_hits <- transform(batter_ball_hits, hit_side = ifelse(ball_position_x < 0, 'L', 'R'))
batter_ball_hits <- transform(batter_ball_hits, push_pull = ifelse(hit_side != batter_hand, 'pull', 'push'))

write.csv(batter_ball_hits, "batter_ball_hits.csv")

hit_outcomes <- batter_ball_hits |>
  group_by(batter,batter_hand) |>
  summarize(
    pull <- sum(push_pull == "pull"),
    push <- sum(push_pull == "push")
  ) 

names(hit_outcomes) <- c("batter", "hand", "pull", "push")

write.csv(hit_outcomes, "hit_outcomes.csv")
