install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("gt")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)

dal <- load_pbp(2022)

View(dal)

# Who was the best runner for Dallas at HOME
dalrushhome <- dal %>%
  filter(posteam == "DAL", home_team == "DAL") %>%
  select(rusher_player_name, yards_gained) %>%
  group_by(rusher_player_name) %>%
  summarize(yards = sum(yards_gained))

dalrushhome %>%
  ggplot(aes(rusher_player_name, yards)) +
  geom_col() +
  labs(x = "Rusher")

#Who was the best runner for DAL AWAY
dalrushaway <- dal %>%
  filter(posteam == "DAL", away_team == "DAL") %>%
  select(rusher_player_name, yards_gained) %>%
  group_by(rusher_player_name) %>%
  summarize(yards = sum(yards_gained))

dalrushaway %>%
  ggplot(aes(rusher_player_name, yards)) +
  geom_col() +
  labs(x = "Rusher")

# Combine the two
dal_all <- dalrushhome %>%
  left_join(dalrushaway, by = "rusher_player_name")

dal_all %>%
  select(rusher_player_name, yards.x, yards.y) %>%
  group_by(rusher_player_name) %>%
  summarize(home_yards = sum(yards.x), away_yards = sum(yards.y)) %>%
  

View(dal_all)


total <- dal_all %>%
  select(rusher_player_name, yards.x, yards.y) %>%
  group_by(rusher_player_name) %>%
  summarize(total_home = sum(yards.x), total_away = sum(yards.y))

View(total)

# Created a new column and dropped NA values
dal_all %>%
  drop_na(yards.x, yards.y)

dal_all <- dal_all %>%
  mutate(total = yards.x + yards.y) %>%
  drop_na(yards.x, yards.y)

#Bar chart
dal_all %>%
  ggplot() +
  geom_col(mapping = aes(x = rusher_player_name, y = total))

# Scatter plot
dal_all %>%
  ggplot() +
  geom_point(mapping = aes(x = yards.x, y = yards.y, color = rusher_player_name))

         