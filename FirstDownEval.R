library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

#code will create a graph of first down success compared to the pass rate 

pbp <- load_pbp(2022)


pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa))

FirstDown <- pbp_rp %>%
  filter(down == 1)%>%
  filter(!is.na(pass_attempt))%>%
  group_by(posteam) %>%
  summarize(SuccessRun = sum(yards_gained >= 5 )/ sum(play) * 100, Pass1Down = mean(pass_attempt) * 100)


FirstDown <- FirstDown %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Now we can make our plot with team logos!
FirstDown %>%
  ggplot(aes(x = Pass1Down, y =Success)) +
  geom_hline(yintercept = mean(FirstDown$Success), linetype = "dashed") +
  geom_vline(xintercept = mean(FirstDown$Pass1Down), linetype = "dashed") +
 
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_bw() +
  labs(x = "Passing Rate On 1st Down",
       y = "Succesful 1st Down Rate (Gain at Least 5 Yards)",
       title = "Passing Rate Compared to Success Rate on 1st Down") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) #put the title in the middle

# Saving the plot
ggsave('FistDwnSucc.png', width = 14, height = 10, dpi = "retina")
