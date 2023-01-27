# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, StatsBombR, here, glue, magick, sysfonts, showtext)

# Fix plot size using camcorder -------------------------------------------
camcorder::gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)

# Load data and set font/theme --------------------------------------------
data <- readRDS(here::here("data/uefa_euro_2020.RDS"))

source(here::here("globals.R"))

sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

theme_set(theme_minimal())
theme_update(
  text = element_text(family = font),
  plot.title = element_text(size = 30, face = "bold"),
  plot.subtitle = element_text(size = 14),
  plot.title.position = "plot",
  legend.position = "top",
  legend.justification = c(-0.02, 1),
  legend.key = element_blank(),
  strip.text = element_text(face = "bold"),
  plot.caption = ggtext::element_markdown()
)


# Data wrangling ----------------------------------------------------------

##subtitle text
sub <-
  "Progressive Pass = A successful open play pass that moves the ball 25% closer to the opposition goal"

##basic data wrangling - adding progressive passes and filtering to open play passes only
df <-
  data |>
  filter(type.name == "Pass") |>
  mutate(pass.outcome.name = ifelse(is.na(pass.outcome.name), "Successful", "Unsuccessful")) |>
  filter(pass.outcome.name == "Successful") |>
  progressive_pass() |>
  filter(is_prog == 1,
         is.na(pass.type.name))

##summary stats for those that have prodiced most progressive passes - top 12 players
summary_stats <-
  df |>
  ##filter location to remove GK/CB
  filter(location.x >= 40) |>
  group_by(player.name, team.name, position.name) |>
  summarise(num_prog_pass = sum(is_prog)) |>
  ungroup() |>
  slice_max(order_by = num_prog_pass, n = 12)

##pull player names
players <- summary_stats |> pull(player.name)

(
  p <-
    df |>
    filter(player.name %in% players,
           location.x >= 40) |>
    left_join(summary_stats |> select(player.name, num_prog_pass), by = "player.name") |>
    mutate(lab = glue::glue("{player.name} // {team.name}")) |>
    
    full_sb() +
    
    geom_point(aes(x = location.x, y = location.y),
               alpha = 0.5) +
    
    geom_segment(
      mapping = aes(
        x = location.x,
        y = location.y,
        xend = pass.end_location.x,
        yend = pass.end_location.y
      ),
      alpha = 0.5,
      arrow = arrow(length = unit("0.06", "inches"))
    ) +
    
    geom_text(
      aes(
        x = 28,
        y = 3,
        label = glue::glue("Progressive Passes: {num_prog_pass}")
      ),
      size = 2.6,
      check_overlap = TRUE
    ) +
    
    labs(
      title = "Top Progressive Passers - Uefa Euros 2020",
      subtitle = str_wrap(sub, width = 60),
      caption = "**Viz**: @AnalyticsOxford<br>**Data**: StatsBomb"
    ) +
    
    facet_wrap(~ lab) +
    
    theme(plot.margin = margin(
      t = 3,
      r = 0,
      b = 35,
      l = 10
    ))
)

##save plot
ggsave(
  plot = p,
  filename = here::here("R", "progressive_passes", glue::glue("euros_progressive_passes.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)

##Load plot and SB logo
plot <-
  here::here("R", "progressive_passes", glue::glue("euros_progressive_passes.jpg"))
logo <- here::here("src", "sb_logo.png")

##position logo on plot
(
  all_plot <- add_logo(
    plot_path = plot,
    logo_path = logo,
    logo_position = "bottom right",
    logo_scale = 6
  )
)

##save complete image
magick::image_write(all_plot, path = here::here("R", "progressive_passes", glue::glue("euros_progressive_passes_full.jpg")))
