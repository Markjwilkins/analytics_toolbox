# Load packages and globals ------------------------------------------------
pacman::p_load(tidyverse,
               worldfootballR,
               camcorder,
               ggpattern,
               ggimage,
               ggsoccer,
               patchwork)

source(here::here("globals.R"))

##get league logos
logos <-
  read_csv(here::here("src", "team_logos", "league_one_2022_2023_logos.csv")) |>
  mutate(image_link = link_to_img(image_link, width = 50))

# Load font ---------------------------------------------------------------
sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

# Load data ---------------------------------------------------------------
match_shots <- read_csv(here::here("data", "league_one_shots.csv"))

##select random match
selected_match <- "3916051"

data <-
  match_shots |>
  filter(match_id == selected_match) |>
  mutate(
    shot_team = ifelse(home_team_id == team_id, home_team, away_team),
    is_goal = ifelse(event_type == "Goal", 1, 0),
    is_goal = as.factor(is_goal)
  )

home <- unique(data$home_team)
away <- unique(data$away_team)

##home/away goals
home_score <- data |>
  filter(shot_team == home,
         is_goal == 1) |>
  count() |>
  pull(n)

away_score <-
  home_score <- data |>
  filter(shot_team == away,
         is_goal == 1) |>
  count() |>
  pull(n)

##home/away logos
home_logo <-
  logos |> filter(fotmob_team_name == home) |> pull(image_link)
away_logo <-
  logos |> filter(fotmob_team_name == away) |> pull(image_link)

summary <-
  data |>
  group_by(shot_team) |>
  summarise(tot_xg = sum(expected_goals, na.rm = TRUE) |> round(2))

##home/away xG
home_xg <- summary |> filter(shot_team == home) |> pull(tot_xg)
away_xg <- summary |> filter(shot_team == away) |> pull(tot_xg)

##home team plot
p1 <-
  plot_international_pitch(orientation = "vertical", type = "offensive_half") +
  
  geom_point(data = data |>
               filter(shot_team == home),
             aes(
               x = x,
               y = y,
               colour = is_goal,
               size = expected_goals
             )) +
  
  geom_point(
    data = data |>
      filter(is_goal == 1,
             shot_team == home),
    aes(x = x,
        y = y,
        size = expected_goals),
    shape = 21,
    fill = "#66C2A5",
    stroke = 0.6,
    colour = "black"
  ) +
  
  scale_colour_manual(
    values = c("#FC8D62", "#66C2A5"),
    name = "Shot Outcome",
    labels = c("No-Goal", "Goal")
  ) +
  
  labs(subtitle = glue::glue("{home_logo}")) +
  
  theme(
    plot.background = element_rect(color = "white"),
    plot.title = ggtext::element_markdown(
      hjust = 0.5,
      face = "bold",
      size = 30,
      family = font
    ),
    plot.margin = margin(2, 1, 1, 1, "cm"),
    legend.box = 'vertical',
    plot.subtitle = ggtext::element_markdown(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "none",
    strip.background = element_blank()
  )

##away team plot
p2 <-
  plot_international_pitch(orientation = "vertical", type = "offensive_half") +
  
  geom_point(data = data |>
               filter(shot_team == away),
             aes(
               x = x,
               y = y,
               colour = is_goal,
               size = expected_goals
             )) +
  
  geom_point(
    data = data |>
      filter(is_goal == 1,
             shot_team == away),
    aes(x = x,
        y = y,
        size = expected_goals),
    shape = 21,
    fill = "#66C2A5",
    stroke = 0.6,
    colour = "black"
  ) +
  
  scale_colour_manual(
    values = c("#FC8D62", "#66C2A5"),
    name = "Shot Outcome",
    labels = c("No-Goal", "Goal")
  ) +
  
  labs(subtitle = glue::glue("{away_logo}")) +
  
  theme(
    plot.background = element_rect(color = "white"),
    plot.title = ggtext::element_markdown(
      hjust = 0.5,
      face = "bold",
      size = 30,
      family = font
    ),
    plot.margin = margin(2, 1, 1, 1, "cm"),
    legend.box = 'vertical',
    plot.subtitle = ggtext::element_markdown(size = 15, hjust = 0.5),
    plot.caption = element_text(size = 10),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "none",
    strip.background = element_blank()
  )

##use patchwork to plot p1/p2
patchwork <- (p1 | p2)

##create title
sub <-
  glue::glue(
    "{home} {home_score}<span style = 'font-size:12pt;'>{home_xg}xG</span><br> {away} {away_score}<span style = 'font-size:12pt;'>{away_xg}xG</span>"
  )

plot <- patchwork + plot_annotation(
  title = sub,
  subtitle = glue::glue(
    "{unique(data$league_name)} // {unique(data$parent_league_season)}"
  ),
  caption = glue::glue("**Viz**: @AnalyticsOxford<br>**Data**: FotMob.com"),
  theme = theme(
    plot.title = ggtext::element_markdown(
      size = 30,
      face = "bold",
      hjust = 0.5,
      halign = 0,
      family = font
    ),
    plot.subtitle = ggtext::element_markdown(
      size = 14,
      hjust = 0.5,
      family = font
    ),
    plot.caption = ggtext::element_markdown(size = 11, family = font)
  )
)

##save plot
ggsave(
  plot = plot,
  filename = here::here("R", "match_shot_plot", "match_shot_map.jpg"),
  height = 6,
  width = 8
)

