

# Fix packages and globals ------------------------------------------------
pacman::p_load(tidyverse, worldfootballR, camcorder)

source(here::here("scripts", "globals.R"))


# Fix plot size using camcorder -------------------------------------------
gg_record(
  dir = file.path(here::here("outputs")),
  # where to save the recording
  device = "png",
  # device to use to save images
  width = 12,
  # width of saved image
  height = 8,
  # height of saved image
  dpi = 300       # dpi to use when saving image
)


# Load font and theming ---------------------------------------------------
sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"


theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  plot.title = ggtext::element_textbox(size = 30, face = "bold"),
  plot.subtitle = ggtext::element_textbox(size = 30, face = "bold"),
  plot.caption = element_text(size = 12),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_text(face = "bold")
)

##get league logos
logos <-
  read_csv(here::here("output", "team_logos", "league_one_2022_2023_logos.csv"))


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

league_logo <-
  glue::glue("https://images.fotmob.com/image_resources/logo/leaguelogo/{league_id}.png")


# Data wrangling ----------------------------------------------------------
##find all matches that have been played
matches <-
  league_json$matches$data$allMatches |>
  filter(status$finished == TRUE)

matches[c("home_team_score", "away_team_score")] <-
  str_split_fixed(string = matches$status$scoreStr, pattern = "-", 2)

matches <-
  matches |>
  mutate(
    home_team_score = str_squish(home_team_score),
    away_team_score = str_squish(away_team_score)
  )

##pull first match ID for example - can add any match ID
# match_id <- matches |> head(1) |> pull(id)

match_id <- 3916155

##pull individual match information
match_shots <-
  worldfootballR::fotmob_get_match_details(match_ids = match_id)

data <-
  match_shots |>
  mutate(
    shot_team = ifelse(team_id == home_team_id, home_team, away_team),
    expected_goals = replace_na(expected_goals, as.numeric(0))
  )

home_team_score <- matches |> filter(id == match_id) |> pull(home_team_score)
away_team_score <- matches |> filter(id == match_id) |> pull(away_team_score)

##minutes played
min_min <- as.numeric(0)
min_max <- unique(as.numeric(pmax(data$min, 90)))

##code for logo plotting
link_to_img <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

match_teams <- tibble(
  home_away = c("home", "away"),
  home_away_team = c(unique(data$home_team), unique(data$away_team)),
  home_away_score = c(
    home_team_score,
    away_team_score
  )
)

team_logo_home <-
  logos |> filter(fotmob_team_name == match_teams$home_away_team[1]) |> pull(image_link)
team_logo_away <-
  logos |> filter(fotmob_team_name == match_teams$home_away_team[2]) |> pull(image_link)

match_teams <-
  match_teams |>
  tibble(logo_to_plot = c(
    link_to_img(x = team_logo_home, width = 40),
    link_to_img(x = team_logo_away, width = 40)
  ))

df <-
  data |>
  add_row(
    min = min_min,
    shot_team = as.character(match_teams$home_away_team[1]),
    expected_goals = as.numeric(0)
  ) |>
  add_row(
    min = min_min,
    shot_team = as.character(match_teams$home_away_team[2]),
    expected_goals = as.numeric(0)
  ) |>
  add_row(
    min = min_max,
    shot_team = as.character(match_teams$home_away_team[1]),
    expected_goals = as.numeric(0)
  ) |>
  add_row(
    min = min_max,
    shot_team = as.character(match_teams$home_away_team[2]),
    expected_goals = as.numeric(0)
  ) |>
  mutate(event_type = ifelse(is_own_goal == TRUE, paste0("Own Goal"), event_type)) |>
  mutate(
    expected_goals = round(expected_goals, 2),
    player_label = dplyr::case_when(
      event_type == "Goal" &
        situation != "Penalty" ~ paste0(player_name, ": ", expected_goals, " xG"),
      event_type == "Goal" &
        situation == "Penalty" ~ paste0(player_name, " (Penalty): ", expected_goals, " xG"),
      event_type == "Own Goal" ~ paste0(player_name, " (Own Goal)"),
      TRUE ~ ""
    )
  ) |>
  arrange(min)

data_home <-
  df |>
  filter(shot_team == match_teams$home_away_team[1]) |>
  mutate(xg_tot = cumsum(expected_goals))

data_away <-
  df |>
  filter(shot_team == match_teams$home_away_team[2]) |>
  mutate(xg_tot = cumsum(expected_goals))

home_team_col <- "#ffdd00"
away_team_col <- "#ff0000"


# PLOT! -------------------------------------------------------------------
(
  p <-
    ggplot() +
    geom_vline(
      xintercept = 45,
      lty = 2,
      colour = "grey80"
    ) +
    geom_step(
      data_home,
      mapping = aes(x = min, y = xg_tot),
      colour = home_team_col,
      size = 2
    ) +
    geom_step(
      data_away,
      mapping = aes(x = min, y = xg_tot),
      colour = away_team_col,
      size = 2
    ) +
    geom_point(
      data_home |> filter(event_type == "Goal" | is_own_goal == TRUE),
      mapping = aes(x = min, y = xg_tot),
      color = home_team_col,
      fill = "white",
      shape = 21,
      stroke = 2,
      size = 3
    ) +
    geom_point(
      data_away |> filter(event_type == "Goal" | is_own_goal == TRUE),
      mapping = aes(x = min, y = xg_tot),
      color = away_team_col,
      fill = "white",
      shape = 21,
      stroke = 2,
      size = 3
    ) +
    ggrepel::geom_label_repel(
      data_home,
      mapping = aes(x = min, y = xg_tot, label = player_label),
      nudge_y = 0.1,
      max.overlaps = 1000
    ) +
    ggrepel::geom_label_repel(
      data_away,
      mapping = aes(x = min, y = xg_tot, label = player_label),
      nudge_y = 0.1,
      max.overlaps = 1000
    ) +
    scale_x_continuous(
      limits = c(min_min, min_max + 6),
      breaks = seq(min_min, min_max + 6, by = 10),
      expand = c(0, 0)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    geom_text(
      data_home,
      mapping = aes(
        x = min_max + 2.5,
        y = max(xg_tot),
        label = paste0(max(round(xg_tot, 3)), " xG")
      ),
      colour = home_team_col,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    geom_text(
      data_away,
      mapping = aes(
        x = min_max + 2.5,
        y = max(xg_tot),
        label = paste0(max(round(xg_tot, 3)), " xG")
      ),
      colour = away_team_col,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    labs(
      title = glue::glue(
        "{match_teams$logo_to_plot[1]}  <span style = 'color:{home_team_col};'> {match_teams$home_away_team[1]}:</span> {match_teams$home_away_score[1]}"
      ),
      subtitle = glue::glue(
        "{match_teams$logo_to_plot[2]}  <span style = 'color:{away_team_col};'> {match_teams$home_away_team[2]}:</span> {match_teams$home_away_score[2]}"
      ),
      x = "Minute",
      y = "Expected Goals",
      caption = "Data: Fotmob.com | Viz: @AnalyticsOxford"
    )
)

home <- match_teams$home_away_team[1] |> str_to_lower() |> str_replace(" ", "_")
away <- match_teams$home_away_team[2] |> str_to_lower() |> str_replace(" ", "_")

##save plot
ggsave(
  plot = p,
  filename = here::here("output", glue::glue("{home}_{away}_xg_timeline.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)


# Add league logo to original plot ----------------------------------------
##Load plot
plot <-
  here::here("output", glue::glue("{home}_{away}_xg_timeline.jpg"))

##position logo on plot
(
  all_plot <- add_logo(
    plot_path = plot,
    logo_path = league_logo,
    logo_position = "top right",
    logo_scale = 10
  )
)

##save complete image
magick::image_write(all_plot, path = here::here(
  "output",
  glue::glue("{home}_{away}_xg_timeline_full.jpg")
))
