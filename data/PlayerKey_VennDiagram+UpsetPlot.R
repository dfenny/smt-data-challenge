# Welcome to the 2024 SMT Data Challenge! Here are some functions to help you get
# started. After you unzip the dataset, copy the name of the directory you saved 
# it to into the 'data_directory` field below. After making sure you have the 
# `arrow` package installed, you may call this file at the top of your work file(s)
# by calling `source("SMT_Data_starter.R"). Then, you may apply functions and 
# operations to the table names below as you would any other table and load them 
# into your working environment by calling `collect()`. For an example of this 
# process, un-comment and run the lines below the starter code. 
# 
# WARNING: The data subsets are large, especially `player_pos`. Reading the 
#   entire subset at once without filtering may incur performance issues on your 
#   machine or even crash your R session. It is recommended that you filter 
#   data subsets wisely before calling `collect()`.

data_directory <- 'C:/Users/jsout/code/smt-data-challenge/data/2024_SMT_Data_Challenge/'


###############################################################################
################## STARTER CODE: DO NOT MODIFY ################################
###############################################################################

library(arrow)

game_info <- arrow::open_csv_dataset(paste0(data_directory,"/game_info"), 
                                     partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

ball_pos <- arrow::open_csv_dataset(paste0(data_directory,"/ball_pos"), 
                                    partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                    hive_style = F, 
                                    unify_schemas = T, 
                                    na = c("", "NA", "NULL", NA, "\\N"))

game_events <- arrow::open_csv_dataset(paste0(data_directory,"/game_events"), 
                                       partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                       hive_style = F, 
                                       unify_schemas = T, 
                                       na = c("", "NA", "NULL", NA, "\\N"))

player_pos <- arrow::open_csv_dataset(paste0(data_directory,"/player_pos"), 
                                      partitioning = c("Season", "HomeTeam", "AwayTeam", "Day"), 
                                      hive_style = F, 
                                      unify_schemas = T, 
                                      na = c("", "NA", "NULL", NA, "\\N"))

team_info <- arrow::open_csv_dataset(paste0(data_directory,"/team_info.csv"), 
                                     hive_style = F, 
                                     unify_schemas = T, 
                                     na = c("", "NA", "NULL", NA, "\\N"))

###############################################################################
########################## END STARTER CODE ###################################
###############################################################################
library(tidyverse)
library(ggVennDiagram)
game_info_demo <- game_info |>
  filter(Day == "day_059",
         inning == 3) |>
  collect()

game_info_select <- game_info |> 
  select(game_str, home_team, top_bottom, pitcher:batter) |> 
  filter(top_bottom == "top") |> 
  collect()

game_info_select_clean <- game_info_select |> 
  distinct(game_str, home_team, top_bottom, pitcher, catcher, first_base, second_base, third_base, shortstop, left_field, center_field, right_field)

# game_info_select <- game_info |> 
#   select(game_str, home_team, top_bottom, pitcher:batter) |> 
#   filter(top_bottom == "top") |> 
#   group_by(game_str) |> 
#   ungroup() |> 
#   collect()

###
pitchers <- game_info_select_clean |>
  select(pitcher) |> 
  as.list(unique(pitchers))
#pitchers_list <- as.list(sort(unique(unlist(pitchers))))
pitchers_list <- sort(unique(unlist(pitchers)))

catchers <- game_info_select_clean |> 
  select(catcher) |> 
  as.list(unique(catcher))
catchers_list <- sort(unique(unlist(catchers)))

firstbasemen <- game_info_select_clean |> 
  select(first_base) |> 
  as.list(unique(first_base))
firstbasemen_list <- sort(unique(unlist(firstbasemen)))

secondbasemen <- game_info_select_clean |> 
  select(second_base) |> 
  as.list(unique(second_base))
secondbasemen_list <- sort(unique(unlist(secondbasemen)))

thirdbasemen <- game_info_select_clean |> 
  select(third_base) |> 
  as.list(unique(third_base))
thirdbasemen_list <- sort(unique(unlist(thirdbasemen)))

shortstops <- game_info_select_clean |> 
  select(shortstop) |> 
  as.list(unique(shortstop))
shortstops_list <- sort(unique(unlist(shortstops)))

leftfielders <- game_info_select_clean |> 
  select(left_field) |> 
  as.list(unique(left_field))
leftfielders_list <- sort(unique(unlist(leftfielders)))

centerfielders <- game_info_select_clean |> 
  select(center_field) |> 
  as.list(unique(center_field))
centerfielders_list <- sort(unique(unlist(centerfielders)))

rightfielders <- game_info_select_clean |> 
  select(right_field) |> 
  as.list(unique(right_field))
rightfielders_list <- sort(unique(unlist(rightfielders)))

### PLOTS
position_venn_diagram <- list(pitchers_list, 
                              catchers_list, 
                              firstbasemen_list, 
                              secondbasemen_list, 
                              thirdbasemen_list,
                              shortstops_list,
                              leftfielders_list,
                              centerfielders_list,
                              rightfielders_list
                              )

fielder_venn <- list(leftfielders_list,
                     centerfielders_list,
                     rightfielders_list)

utility_venn <- list(secondbasemen_list,
                   thirdbasemen_list,
                   leftfielders_list,
                   centerfielders_list)

ss_2B_venn <- list(secondbasemen_list,
                   shortstops_list)

ggVennDiagram(position_venn_diagram,
              category.names = c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"))

ggVennDiagram(fielder_venn,
              category.names = c("LF", "CF", "RF"))

ggVennDiagram(utility_venn,
              category.names = c("2B", "3B", "LF", "CF"))

ggVennDiagram(ss_2B_venn,
              category.names = c("2B", "SS"))

intersect(pitchers_list, catchers_list)
################################################################################

# team_info_demo <- team_info |>
#   #filter(player_id == 733) |>
#   collect()

team_info_select <- team_info |> 
  collect() |> 
  group_by(player_id) |> 
  summarize(Number_1A_Series = sum(home_team == "Home1A"),
            Number_2A_Series = sum(home_team == "Home2A"),
            Number_3A_Series = sum(home_team == "Home3A"),
            Number_4A_Series = sum(home_team == "Home4A"))
# ^^ https://stackoverflow.com/questions/28195996/count-number-of-rows-matching-a-criteria

# team_info_select <- team_info_select |> 
#   mutate("Position(s)" = )

all_player_list <- c(pitchers, catchers, firstbasemen, secondbasemen, thirdbasemen, shortstops, leftfielders, centerfielders, rightfielders)
all_player_list_clean <- all_player_list |> 
  filter(complete.cases(all_player_list))

all_player_df <- as.data.frame.list(all_player_list)
unique(all_player_df$pitcher)
unique(pitchers)

pitcher_df <- all_player_df |> expand(pitcher)
pitcher_df <- pitcher_df |> mutate("Position.P" = "P") |> rename("player_id" = "pitcher")

catcher_df <- all_player_df |> expand(catcher)
catcher_df <- catcher_df |> mutate("Position.C" = "C") |> rename("player_id" = "catcher")

merge_df <- merge(pitcher_df, catcher_df, by = "player_id", all = TRUE)
# https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right

firstbasemen_df <- all_player_df |> expand(first_base)
firstbasemen_df <- firstbasemen_df |> mutate("Position.1B" = "1B") |> rename("player_id" = "first_base")
merge_df <- merge(merge_df, firstbasemen_df, by = "player_id", all = TRUE)

secondbasemen_df <- all_player_df |> expand(second_base)
secondbasemen_df <- secondbasemen_df |> mutate("Position.2B" = "2B") |> rename("player_id" = "second_base")
merge_df <- merge(merge_df, secondbasemen_df, by = "player_id", all = TRUE)

thirdbasemen_df <- all_player_df |> expand(third_base)
thirdbasemen_df <- thirdbasemen_df |> mutate("Position.3B" = "3B") |> rename("player_id" = "third_base")
merge_df <- merge(merge_df, thirdbasemen_df, by = "player_id", all = TRUE)

shortstop_df <- all_player_df |> expand(shortstop)
shortstop_df <- shortstop_df |> mutate("Position.SS" = "SS") |> rename("player_id" = "shortstop")
merge_df <- merge(merge_df, shortstop_df, by = "player_id", all = TRUE)

leftfielder_df <- all_player_df |> expand(left_field)
leftfielder_df <- leftfielder_df |> mutate("Position.LF" = "LF") |> rename("player_id" = "left_field")
merge_df <- merge(merge_df, leftfielder_df, by = "player_id", all = TRUE)

centerfielder_df <- all_player_df |> expand(center_field)
centerfielder_df <- centerfielder_df |> mutate("Position.CF" = "CF") |> rename("player_id" = "center_field")
merge_df <- merge(merge_df, centerfielder_df, by = "player_id", all = TRUE)

rightfielder_df <- all_player_df |> expand(right_field)
rightfielder_df <- rightfielder_df |> mutate("Position.RF" = "RF") |> rename("player_id" = "right_field")
merge_df <- merge(merge_df, rightfielder_df, by = "player_id", all = TRUE)

merge_df$AllPosition <- apply(merge_df, 1, function(x) paste(x[!is.na(x)], collapse = ", "))
# https://stackoverflow.com/questions/47763452/r-collapsing-data-from-multiple-columns-into-one
#merge_df <- 0

# make a new df of the nine positions and all player ids who played them?
# all_player_list |>
#   pivot_longer(names_to = "Position", values_to = player_id)
# #all_player_list2 <- c(pitchers_list, catchers_list, firstbasemen_list, secondbasemen_list, thirdbasemen_list, shortstops_list, leftfielders_list, centerfielders_list, rightfielders_list)

df_farm_system_players <- merge(team_info_select, merge_df, by = "player_id", all = TRUE)
df_farm_system_players <- select(df_farm_system_players, "player_id":"Number_4A_Series", "AllPosition") #|> rename("Positions_Played" = "AllPosition")


# remove player_id that are 1000 or greater
df_farm_clean <- df_farm_system_players[df_farm_system_players$player_id < 1000, ]
# remove player_id "NA" at bottom of df 
#################################################### NOT WORKING
df_farm_clean <- df_farm_clean[df_farm_clean$player_id != NA]
df_farm_clean <- df_farm_clean[df_farm_clean$player_id[!is.na(df_farm_clean$player_id)]]
df_farm_clean <- df_farm_clean[omit(is.na(df_farm_clean$player_id))]

print(df_farm_clean$player_id[is.na(df_farm_clean$player_id)])
print(df_farm_clean$player_id[!is.na(df_farm_clean$player_id)])

# remove the player_id number from the first element of the AllPosition column (covert string to list)
########################################### PARTIALLY WORKING
df_farm_clean$AllPosition[1]
df_farm_clean$AllPosition[1][1] # the AllPosition column is one long string, need to split
df_farm_clean$AllPosition <- str_trim(df_farm_clean$AllPosition)
df_farm_clean$AllPosition <- gsub("\\s+", "", df_farm_clean$AllPosition)
#https://stackoverflow.com/questions/19128327/how-to-remove-extra-white-space-between-words-inside-a-character-vector-using
df_farm_clean$AllPosition <- str_split(df_farm_clean$AllPosition, pattern = ",")
df_farm_clean$AllPosition[[1]][1]
df_farm_clean$AllPosition[[1]][2]

loopsetup <- df_farm_clean$AllPosition[[]]
loopsetup <- df_farm_clean

for (i in loopsetup){
  remove(x[1])
}

for (i in loopsetup){
  temp <-  collect("AllPosition"[[i]])
  remove(temp[1])
}

