library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

#Code will get the amount of points scored and allowed for each team during the middle 8 minutes of the game 

pbp <- load_pbp(2022)


#Get the points scored
pbpMiddleEightO <- pbp %>%
  filter(!is.na(epa))%>%
  mutate(posteamPointsScores = posteam_score_post - posteam_score)%>%
  filter(game_seconds_remaining <= 2040 & game_seconds_remaining >=1560)%>%
  group_by(posteam)%>%
  summarise(PointsScored = sum(posteamPointsScores))


#get the points allowed
pbpMiddleEightD <- pbp %>%
  filter(!is.na(epa))%>%
  mutate(posteamPointsScores = posteam_score_post - posteam_score)%>%
  filter(game_seconds_remaining <= 2040 & game_seconds_remaining >=1560)%>%
  group_by(defteam)%>%
  summarise(PointsAllowed = sum(posteamPointsScores))

#change column names
colnames(pbpMiddleEightO) <- c("Team" , "PointsScored")
colnames(pbpMiddleEightD) <- c("Team" , "PointsAllowed")


#joing dfs
pbpMiddleEight <- merge(pbpMiddleEightO,pbpMiddleEightD, by = c("Team") )

pbpMiddleEight <- pbpMiddleEight %>% mutate(PointDifferential = PointsScored - PointsAllowed)

