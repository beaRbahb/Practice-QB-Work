# Load packages
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggdist)
library(rcartocolor)
library(nflplotR)
library(ggplot2)
library(ggtext)

#load 2021 data
data2021 <- load_pbp(2021)

#Set rookie group
rookieQB2021 <- c("J.Fields", "Z.Wilson", "T.Lance", "T.Lawrence", "M.Jones", "D.Mills")

#Filter out the data for our rookie group on dropbacks and pull data for the graph
qbEPA2021 <- data2021 %>%
  filter(passer %in% rookieQB2021) %>%
  filter(qb_dropback == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>%
  group_by(passer, week, posteam, id) %>%
  summarize(qbEPA = epa, 
            n_dropbacks = sum(pass)) %>%
  ungroup() %>%
  filter(n_dropbacks >= 10)

#add in the colors for the teams for the graph
rookieQBEPA2021 <- qbEPA2021 %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
  
#Big time raincloud plot via https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/ 
ggplot(rookieQBEPA2021, aes(x = passer, y = qbEPA)) + 
  ggdist::stat_halfeye(
    aes(fill = team_color),
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA, 
    alpha = 0.7
    ) + 
  geom_boxplot(
    width = .2, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 2,
    alpha = .3,
    color = rookieQBEPA2021$team_color2,
    fill = rookieQBEPA2021$team_color,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  scale_color_identity() +
  scale_fill_identity() +
  facet_grid() +
  coord_flip() + 
  geom_hline(yintercept = mean(qbEPA2021$qbEPA), linetype = "dashed", alpha = 0.5) +
  theme_minimal() + 
  labs(
    title = glue::glue("Rookie QB EPA Distributions through Week 7"),
    axis.text.x = element_blank(), 
    y = "EPA/Play") + 
  theme(axis.title.y = element_blank())
  
ggsave('RookieEPAThroughWeek7.png', width = 15, height = 10, dpi = "retina")
