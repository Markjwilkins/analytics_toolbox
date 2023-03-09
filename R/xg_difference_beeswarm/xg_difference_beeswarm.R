# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, worldfootballR, ggtext, magick, patchwork, here, glue)

##import globals
source(here::here("globals.R"))

##get league logos
logos <-
  read_csv(here::here("src", "team_logos", "league_one_2022_2023_logos.csv"))


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
  leagues |> filter(country == "England", name == "League One") |> pull(id)

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
  league_json$matches$allMatches |>
  filter(status$finished == TRUE)

##played matches match id
played_matches <- unique(matches$id)

##pull individual match information
match_shots <-
  worldfootballR::fotmob_get_match_details(match_ids = played_matches)

##save shots .csv
# write_csv(match_shots, here::here("data", "league_one_shots.csv"))

##create total home and away xg for each match
match_summary <-
  match_shots |>
  mutate(shot_team_name = ifelse(team_id == home_team_id, home_team, away_team)) |>
  group_by(match_id, match_round, home_team, away_team, shot_team_name) |>
  summarise(
    home_xg = sum(expected_goals[home_team == shot_team_name], na.rm = TRUE),
    away_xg = sum(expected_goals[away_team == shot_team_name], na.rm = TRUE)
  ) |>
  ungroup()

##home xg
home <-
  match_summary |>
  filter(home_xg > 0) |>
  select(-away_xg)

##away xg
away <-
  match_summary |>
  filter(away_xg > 0) |>
  select(-home_xg)

##combine data and reshape
combo_data <-
  left_join(home, away, by = "match_id") |>
  select(
    match_id,
    home = home_team.x,
    away = away_team.x,
    home_xg,
    away_xg,
    match_round = match_round.x
  ) |>
  pivot_longer(c("home", "away"), names_to = "home_away", values_to = "team") |>  # pivot from wide to long format
  mutate(
    # from home/away to for/against
    xg_for = ifelse(home_away == "home", home_xg, away_xg),
    xg_against = ifelse(home_away == "home", away_xg, home_xg),
    .keep = "unused"
  )

##join logos and set some aesthetics
all_combo <-
  combo_data |>
  mutate(match_round = as.numeric(match_round)) |>
  arrange(match_round) |>
  mutate(
    xg_diff = xg_for - xg_against |> round(2),
    col = ifelse(xg_diff < 0, "#1e466e", "#e76254")
  ) |>
  left_join(logos |> select(team = fotmob_team_name, image_link), by = "team") |>
  mutate(image_link = link_to_img(image_link, width = 23))

##unique teams in data set to iterate over and create beeswarm plot for
unique_teams <- sort(unique(all_combo$team))

##function to generate individual team beeswarm
plot_fun <- function(team_name) {
  dat <-
    all_combo |>
    filter(team == team_name)
  
  dat |>
    ggplot(aes(x = xg_diff, y = team, fill = xg_diff)) +
    geom_vline(xintercept = 0, linewidth = 1) +
    ggbeeswarm::geom_quasirandom(shape = 21,
                                 colour = "black",
                                 size = 3) +
    coord_fixed() +
    scale_x_continuous(
      breaks = seq(from = -4.5, to = 4.5, by = 1.5),
      limits = c(-4.5, 4.5),
      expand = c(0, 0),
      labels = function(x)
        ifelse(x > 0, paste0("+", x), x)
    ) +
    scale_fill_gradient2(low = "#1e466e", mid = "white", high = "#e76254") +
    theme_minimal() +
    labs(
      title = glue::glue("{team_name}<br>{dat$image_link}"),
      x = "",
      y = ""
    ) +
    theme(
      plot.title = ggtext::element_markdown(
        size = 14,
        hjust = 0.5,
        family = font,
        face = "bold"
      ),
      axis.text.y = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(size = 7)
    )
}

##test function with single team
plot_fun(team_name = unique_teams[1])

##iterate over all teams to create plots within a list
all_plots <-
  unique_teams |> 
  map( ~ plot_fun(team_name = .x))

(p <- patchwork::wrap_plots(all_plots, ncol = 4, nrow = 6) +
  patchwork::plot_annotation(
    title = 'Match xG Difference',
    subtitle = "League One // 2022/2023 <br>",
    caption = glue::glue("**Data**: FotMob.com<br>**Viz**: @AnalyticsOxford"),
    theme = theme(
      plot.title = element_text(
        size = 50,
        family = font,
        face = "bold"
      ),
      plot.subtitle = ggtext::element_markdown(size = 18, family = font),
      plot.title.position = "plot",
      plot.caption = ggtext::element_markdown(family = font, size = 12)
    )
  ))

ggsave(
  plot = p,
  filename = here::here("R", "xg_difference_beeswarm", "league_one_xg_beeswarm.jpg"),
  dpi = 300,
  width = 17,
  height = 12
)

##import image
plot <- here::here("R", "xg_difference_beeswarm", "league_one_xg_beeswarm.jpg")

##position logo on plot
(
  all_plot <- add_logo(
    plot_path = plot,
    logo_path = league_logo,
    logo_position = "top right",
    logo_scale = 15
  )
)

##save complete image
magick::image_write(
  all_plot,
  path = here::here(
    "R",
    "xg_difference_beeswarm",
    "league_one_xg_beeswarm_full.jpg"
  )
)

