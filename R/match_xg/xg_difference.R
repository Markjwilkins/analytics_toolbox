# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, worldfootballR, gradienttext, ggtext, magick)

##https://github.com/samiaab1990/gradienttext

# Fix plot size using camcorder -------------------------------------------
camcorder::gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)


# Load font ---------------------------------------------------------------
sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

# Load data ---------------------------------------------------------------
##find and select league id
leagues <- worldfootballR::fotmob_get_league_ids()

##find and pull EPL league code
league_id <-
  leagues |> filter(country == "England", name == "League Two") |> pull(id)

##base url
url <- "https://www.fotmob.com/api/leagues?id="

##url with league code
complete_url <- glue::glue("{url}{league_id}")

##pull league JSON
league_json <- jsonlite::fromJSON(complete_url)

##league logo for final plot
league_logo <-
  glue::glue("https://images.fotmob.com/image_resources/logo/leaguelogo/{league_id}.png")

##pull played matches
matches <-
  league_json$matches$data$allMatches |>
  filter(status$finished == TRUE)

##played matches match id
played_matches <- unique(matches$id)

##pull individual match information
match_shots <-
  worldfootballR::fotmob_get_match_details(match_ids = played_matches)

##league name for saving
league_name <-
  str_to_lower(unique(match_shots$league_name)) |> str_replace(" ", "_")

league_title <- unique(match_shots$league_name)

##create total home and away xg for each match
match_summary <-
  match_shots |>
  mutate(shot_team_name = ifelse(team_id == home_team_id, home_team, away_team)) |>
  group_by(match_id, home_team, away_team, shot_team_name) |>
  summarise(home_xg = sum(expected_goals[home_team == shot_team_name]),
            away_xg = sum(expected_goals[away_team == shot_team_name]))

home <-
  match_summary |>
  filter(home_xg > 0) |>
  select(-away_xg)

away <-
  match_summary |>
  filter(away_xg > 0) |>
  select(-home_xg)

##combine data and reshape
combo_data <-
  left_join(home, away, by = "match_id") |>
  select(match_id,
         home = home_team.x,
         away = away_team.x,
         home_xg,
         away_xg) |>
  pivot_longer(c("home", "away"), names_to = "home_away", values_to = "team") |>  # pivot from wide to long format
  mutate(
    # from home/away to for/against
    xg_for = ifelse(home_away == "home", home_xg, away_xg),
    xg_against = ifelse(home_away == "home", away_xg, home_xg),
    .keep = "unused"
  )

##colour palette for the title and plot
cols <- c("#e76254", "#1e466e")

##subtitle string
sub <-
  "Each dot reprents a match in the 2022/2023 season. Those points above <br>the reference line indicate the team has conceded
more xG than created. <br>Those points below the line show an xG difference in the teams favour."


# PLOT! -------------------------------------------------------------------
(
  p <-
    combo_data |>
    ggplot(aes(
      x = xg_for,
      y = xg_against,
      fill = (xg_for - xg_against)
    )) +
    geom_point(
      shape = 21,
      size = 2.6,
      stroke = 0.8
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linewidth = 0.1
    ) +
    labs(
      title = gradienttext::make_gradient(glue::glue("{league_title} xG By Game"), cols),
      subtitle = sub,
      x = "xG For",
      y = "xG Against",
      caption = "**Viz**: @AnalyticsOxford<br>**Data**: Fotmob.com"
    ) +
    facet_wrap(. ~ team, ncol = 6, nrow = 4) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 10, face = "bold"),
      plot.title = ggtext::element_markdown(size = 40, face = "bold"),
      plot.subtitle = element_text(size = 14, face = "bold"),
      plot.title.position = "plot",
      axis.title = element_text(face = "bold"),
      plot.caption = ggtext::element_markdown()
    ) +
    scale_fill_gradient2(low = cols[1], high = cols[2])
)

##save plot
ggsave(
  plot = p,
  filename = here::here("R", "match_xg", glue::glue("{league_name}_xg_per_game.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)

# Add logo to plot --------------------------------------------------------

##Load plot
plot <-
  here::here("R", "match_xg", glue::glue("{league_name}_xg_per_game.jpg")) |> magick::image_read()

##add logo
final <- cowplot::ggdraw() +
  cowplot::draw_image(plot) +
  cowplot::draw_image(league_logo,
                      scale = 0.12,
                      x = 0.43,
                      y = 0.43)

ggsave(plot = final, 
       filename = here::here("R", "match_xg", glue::glue("{league_name}_xg_per_game_full.jpg")),
       height = 8,
       width = 12,
       dpi = 300)



