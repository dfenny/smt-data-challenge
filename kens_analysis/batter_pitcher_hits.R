library(ggplot2)
library(arrow)
library(dplyr)
library(patchwork)

batter_pitch_info <- read.csv("combined_batter_hits.csv")
pitcher_pitch_info <- read.csv("../dougs_analysis/pitcher-location.csv")

