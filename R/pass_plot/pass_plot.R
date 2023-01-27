##Load packages
pacman::p_load(tidyverse, StatsBombR, here, glue, magick, sysfonts, showtext, camcorder)

# Load data and set font/theme --------------------------------------------
##load data already saved from within gloabals.R
data <- readRDS(here::here("data/uefa_euro_2020.RDS"))

##import globals
source(here::here("globals.R"))

# Fix plot size using camcorder -------------------------------------------
camcorder::gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)

##font
sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

##set theme
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

##Select player
player_name <- "Luke Shaw"

##adjust string for saving
player_title <-
  player_name |> str_to_lower() |> str_replace(" ", "_")

##basic data wrangling
df <-
  data |>
  filter(type.name == "Pass",
         player.name == player_name) |>
  mutate(pass.outcome.name = ifelse(is.na(pass.outcome.name), "Successful", "Unsuccessful"))

##player pass summary stats
# summary <-
#   df |>
#   group_by(OpposingTeam) |>
#   summarise(total_pass = n(),
#             total_succ = length(which(pass.outcome.name == "Successful")),
#             succ_pass_pc = total_succ/total_pass*100)

##PLOT!
(
  p <-
    df |>
    full_sb() +
    geom_point(
      aes(x = location.x, y = location.y, colour = pass.outcome.name),
      alpha = 0.5
    ) +
    geom_segment(
      mapping = aes(
        x = location.x,
        y = location.y,
        xend = pass.end_location.x,
        yend = pass.end_location.y,
        colour = pass.outcome.name
      ),
      alpha = 0.5,
      arrow = arrow(length = unit("0.06", "inches"))
    ) +
    scale_color_manual(values = c("#1e466e", "#e76254"), name = "") +
    labs(
      title = glue::glue("{player_name} - Passes vs Opponents"),
      subtitle = glue::glue("{unique(df$team.name)} // Uefa Euros 2020"),
      caption = "**Viz**: @AnalyticsOxford<br>**Data**: StatsBomb"
    ) +
    facet_wrap(~ OpposingTeam)
)

##save plot
ggsave(
  plot = p,
  filename = here::here("R", "pass_plot", glue::glue("{player_title}_passes.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)

# Add logo to plot --------------------------------------------------------

##Load plot and SB logo
plot <-
  here::here("R", "pass_plot", glue::glue("{player_title}_passes.jpg"))
logo <- here::here("src", "sb_logo.png")

##position logo on plot
(all_plot <-
  add_logo(
    plot_path = plot,
    logo_path = logo,
    logo_position = "bottom right",
    logo_scale = 6
  ))

##save complete image
magick::image_write(all_plot, path = here::here(
  "R",
  "pass_plot",
  glue::glue("{player_title}_passes_full.jpg")
))
