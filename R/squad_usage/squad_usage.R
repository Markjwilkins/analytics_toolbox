# Load packages -----------------------------------------------------------
pacman::p_load(worldfootballR, tidyverse, rcartocolor, ggtext, camcorder, janitor)

##load globals
source(here::here("globals.R"))

# Fix plot size using camcorder -------------------------------------------
gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)


# Font and theme ----------------------------------------------------------
sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

theme_set(theme_minimal())


# Data --------------------------------------------------------------------
##load matches for the league
matches <-
  worldfootballR::fb_match_urls(
    country = "ENG",
    gender = "M",
    season_end_year = 2023,
    tier = "3rd"
  ) |>
  tibble() |>
  rename("match_link" = 1) |>
  filter(str_detect(match_link, "Oxford"))

##load player minutes
mins <-
  worldfootballR::fb_match_lineups(match_url = matches$match_link) |>
  janitor::clean_names()

##select team
team_name <- "Oxford United"

##filter minutes for selected team
filtered_mins <-
  mins |>
  select(player_name, min, team, matchday, match_url) |>
  filter(team == team_name)

filtered_mins$matchday <- as.character(filtered_mins$matchday)

players <- unique(filtered_mins$player_name)
match_dates <- unique(filtered_mins$matchday)

##create all matchday possibilities for all squad players
dat <-
  crossing(players, match_dates) |>
  rename("player_name" = players, "matchday" = match_dates)

dat_join <-
  full_join(filtered_mins, dat, by = c("player_name", "matchday"))

##replace na values with 0
dat_join$min[is.na(dat_join$min)] <- 0

##find match days and dates
match_dates <-
  tibble(matchday = unique(dat_join$matchday)) |> rownames_to_column(var = "match")

##join
data_combo <-
  left_join(dat_join, match_dates, by = "matchday") |>
  arrange(match) |>
  mutate(match = as.numeric(match))

##create player total minutes played in the season
summary <-
  data_combo |>
  group_by(player_name) |>
  summarise(total_mins = sum(min, na.rm = TRUE))

data_combo <-
  left_join(data_combo, summary, by = "player_name")

##save file to .csv
# to_save <-
#   data_combo |>
#   select(-team)
#
# write_csv(x = to_save, file = here::here("data", "oufc_squad_usage.csv"))

##create tags for ggtext on y axis
full_data <-
  data_combo |>
  mutate(
    player_tag = glue::glue(
      "<span style = 'font-size:10pt;'>{player_name}</span>
                                  <span style = 'font-size:8pt; color:#e76254'>({total_mins})</span> "
    )
  )


# PLOT! -------------------------------------------------------------------
(
  p <-
    full_data |>
    ggplot(aes(
      x = match,
      y = forcats::fct_rev(player_tag),
      fill = min
    )) +
    geom_tile(aes(height = 0.9, width = 0.9)) +
    geom_text(aes(label = min), size = 3) +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        keywidth = 3,
        nrow = 1,
        title = "Minutes Played"
      )
    ) +
    rcartocolor::scale_fill_carto_c(palette = "Teal",
                                    breaks = c(0, 15, 30, 45, 60, 75, 90)) +
    labs(
      title = "Oxford United - Player Utilisation",
      subtitle = glue::glue(
        "League One // 2022/2023 // <span style = 'color:#e76254'>Total Minutes Played</span>"
      ),
      x = "Matchday",
      y = "",
      caption = "**Viz**: @AnalyticsOxford<br>**Data**: Fotmob.com"
    ) +
    scale_x_continuous(expand = c(0, 1),
                       breaks = seq(1, max(full_data$match), by = 1)) +
    theme(
      text = element_text(family = font),
      legend.position = "top",
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
      plot.subtitle = ggtext::element_textbox(size = 15, hjust = 0.5),
      plot.caption = ggtext::element_markdown(),
      panel.grid = element_blank(),
      axis.text = element_text(face = "bold"),
      axis.text.y = ggtext::element_markdown()
    )
)

##plot title
title <- team_name |> str_to_lower() |> str_replace(" ", "_")

##save plot
ggsave(
  plot = p,
  filename = here::here("R",
                        "squad_usage",
                        glue::glue("{title}_squad_usage.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)


# Load plot and league logos ----------------------------------------------
plot <-
  here::here("R",
             "squad_usage",
             glue::glue("{title}_squad_usage.jpg"))

logo <-
  readr::read_csv(file = here::here("src/team_logos/league_one_2022_2023_logos.csv")) |>
  filter(fotmob_team_name == team_name)

logo_link <- logo |> pull(image_link)

##position logo on plot
(
  all_plot <- add_logo(
    plot_path = plot,
    logo_path = logo_link,
    logo_position = "top left",
    logo_scale = 10
  )
)

##save complete image
magick::image_write(all_plot, path = here::here(
  "R",
  "squad_usage",
  glue::glue("{title}_squad_usage_full.jpg")
))
