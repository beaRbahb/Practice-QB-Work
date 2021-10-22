# Load packages
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
install.packages("ggdist")
library(ggdist)
install.packages("rcartocolor")
library(rcartocolor)

my_pal <- rcartocolor::carto_pal(n = 8, name = "Bold")[c(1, 3, 7, 2)]

theme_flip <-
  theme(
    panel.grid.major.x = element_line(color = "grey90", size = .6),
    panel.grid.major.y = element_blank(),
    legend.position = "top", 
    legend.text = element_text(family = "Roboto Mono", size = 18),
    legend.title = element_text(face = "bold", size = 18, margin = margin(b = 25))
  )

data2021 <- load_pbp(2021)

rookieQB2021 <- c("J.Fields", "Z.Wilson", "T.Lance", "T.Lawrence", "M.Jones")

qbEPA2021 <- data2021 %>%
  filter(passer %in% rookieQB2021) %>%
  filter(qb_dropback == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>%
  group_by(passer, week, posteam) %>%
  summarize(qbEPA = epa, 
            n_dropbacks = sum(pass)) %>%
  ungroup() %>%
  filter(n_dropbacks >= 10)

rookieQBEPA2021 <- qbEPA2021 %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) 

ggplot(rookieQBEPA2021, aes(x = passer, y = qbEPA)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA 
    ) + 
  geom_boxplot(
    width = .15, 
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
  coord_flip() + 
  facet_grid() + 
  geom_hline(yintercept = mean(qbEPA2021$qbEPA), linetype = "dashed") +
  theme_flip