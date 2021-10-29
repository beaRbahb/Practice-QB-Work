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

rookieTeamColor2021 <- rookieQBEPA2021 %>%
  filter(passer %in% rookieQB2021) %>%
  group_by(passer) %>%
  summarize(team_color) %>%
  distinct()

test <- data2021 %>%
  filter(pass == 1) %>%
  group_by(id) %>%
  summarize(qbName = passer)

qbEPA2021 <- data2021 %>%
  filter(passer %in% rookieQB2021) %>%
  filter(qb_dropback == 1, season_type == "REG", !is.na(epa), !is.na(posteam), posteam != "") %>%
  group_by(passer, week, posteam, id) %>%
  summarize(qbEPA = epa, 
            n_dropbacks = sum(pass)) %>%
  ungroup() %>%
  filter(n_dropbacks >= 10)

rookieQBEPA2021 <- qbEPA2021 %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))
  
weeklyRookieEPA2021 <- rookieQBEPA2021[c("passer", "qbEPA", "team_color")]

ggplot2::ggplot(rookieQBEPA2021, aes(x = id, y = qbEPA)) + 
  ggdist::stat_halfeye(
    aes(fill = team_color),
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
  nflplotR::scale_x_nfl_headshots() + 
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_identity() +
  coord_flip() + 
  facet_grid() + 
  geom_hline(yintercept = mean(qbEPA2021$qbEPA), linetype = "dashed", alpha = 0.5) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    axis.title.y = ggplot2::element_blank()
  )
  ggplot2::labs(
    title = "Rookie QB EPA Distributions through Week 7",
    y = "EPA/Play"
  )+
  nflplotR::theme_x_nfl()+
  theme_flip()
 
