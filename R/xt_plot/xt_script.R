# Load libraries ----------------------------------------------------------
pacman::p_load(tidyverse, ggshakeR)

# Load data and globals --------------------------------------------
data <- readRDS(here::here("data/uefa_euro_2020.RDS"))

source(here::here("globals.R"))

# Fix plot size using camcorder -------------------------------------------
camcorder::gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)


# data --------------------------------------------------------------------

##filter for successful open play passes only
df <-
  data |>
  rename(
    "x" = "location.x",
    "y" = "location.y",
    "finalX" = "pass.end_location.x",
    "finalY" = "pass.end_location.y"
  ) |>
  mutate(pass.outcome.name = ifelse(is.na(pass.outcome.name), "successful", "unsuccessful")) |>
  filter(type.name == "Pass",
         pass.outcome.name == "successful",
         is.na(pass.type.name))

##calculate xT of each pass using ggshakeR
xt_data <-
  calculate_threat(data = df, type = "statsbomb") |>
  mutate(xt = xTEnd - xTStart)

##find top 10 players for xT value
top <-
  xt_data |>
  group_by(player.name) |>
  summarise(xt_tot = sum(xt, na.rm = TRUE)) |>
  ungroup() |>
  slice_max(order_by = xt_tot, n = 10)

##Pull top 10 player names
t_players <- unique(top$player.name)

##select top player
top_ind <- t_players |> head(1)


# PLOT! -------------------------------------------------------------------

(
  p <-
    xt_data |>
    filter(player.name %in% top_ind) |>
    ggplot() +
    stat_summary_2d(
      aes(x = x, y = y, z = xt),
      binwidth = c(10, 10),
      fun = function(x)
        sum(x)
    ) +
    ggsoccer::annotate_pitch(dimensions = pitch_statsbomb, fill = NA) +
    theme_pitch() +
    scale_fill_gradient(low = "white", high = "#1e466e", name = "Total xT") +
    guides(
      fill = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        label.position = "bottom",
        keywidth = 3,
        nrow = 1,
        title = "Zone xT Value"
      )
    ) +
    labs(
      title = glue::glue("{top_ind} Expected Threat"),
      subtitle = glue::glue("Uefa Euros 2020<br>"),
      caption = "**Viz**: @AnalyticsOxford<br>**Data**: StatsBomb"
    ) +
    scale_y_reverse() +
    theme(
      legend.position = "top",
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
      plot.subtitle = ggtext::element_markdown(size = 14, hjust = 0.5),
      plot.caption = ggtext::element_markdown(size = 12),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(
        t = 3,
        r = 0,
        b = 35,
        l = 10
      )
    )
)

##save plot
ggsave(
  plot = p,
  filename = here::here("R", "xt_plot", glue::glue("{top_ind}_xt_plot.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)

##Load plot and SB logo
plot <-
  here::here("R", "xt_plot", glue::glue("{top_ind}_xt_plot.jpg"))
logo <- here::here("src", "sb_logo.png")

all_plot <-
  cowplot::ggdraw() +
  cowplot::draw_image(plot) +
  cowplot::draw_image(logo,
                      x = 0.27,
                      y = -0.46,
                      scale = 0.15)

##save complete image
ggsave(
  plot = all_plot,
  filename = here::here("R", "xt_plot", glue::glue("{top_ind}_xt_plot_full.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)
