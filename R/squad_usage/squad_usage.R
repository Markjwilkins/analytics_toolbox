pacman::p_load(worldfootballR, tidyverse, rcartocolor, ggtext, camcorder)

source(here::here("globals.R"))

# Fix plot size using camcorder -------------------------------------------
gg_record(
  dir = file.path(here::here("camcorder_outputs")),
  device = "png",
  width = 12,
  height = 8,
  dpi = 300 
)

sysfonts::font_add_google(name = "IBM Plex Sans", family = "IBM")

showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

font <- "IBM"

theme_set(theme_minimal())

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

mins <- worldfootballR::fb_match_lineups(match_url = matches$match_link) |>
  janitor::clean_names()

team_name <- "Oxford United"

new <-
  mins |>
  mutate(
    match_url = str_remove(match_url, "https://fbref.com/en/matches/")
  ) |>
  select(player_name, min, team, matchday, match_url) |>
  filter(team == team_name)

new$matchday <- as.character(new$matchday)

players <- unique(new$player_name)
match_dates <- unique(new$matchday)

dat <-
  crossing(players, match_dates) |> rename("player_name" = players, "matchday" = match_dates)

new_1 <- full_join(new, dat, by = c("player_name", "matchday"))

new_1$min[is.na(new_1$min)] <- 0

m <-
  tibble(matchday = unique(new_1$matchday)) |> rownames_to_column(var = "match")

new_1 <-
  left_join(new_1, m, by = "matchday") |> arrange(match) |>
  mutate(match = as.numeric(match))

summary <- 
  new_1 |> 
  group_by(player_name) |> 
  summarise(total_mins = sum(min, na.rm = TRUE))

new_1 <- 
  left_join(new_1, summary, by = "player_name")

data <- 
  new_1 |> 
  mutate(player_tag = glue::glue("<span style = 'font-size:10pt;'>{player_name}</span>
                                  <span style = 'font-size:8pt; color:#e76254'>({total_mins})</span> "))

(p <- 
  data |>
  ggplot(aes(x = match, y = forcats::fct_rev(player_tag), fill = min)) +
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
    subtitle = glue::glue("League One // 2022/2023 // <span style = 'color:#e76254'>Total Minutes Played</span>"),
    x = "Matchday",
    y = "",
    caption = "Data: Fotmob.com | Viz: @AnalyticsOxford"
  ) +
  scale_x_continuous(expand = c(0, 1),
                     breaks = seq(1, max(new_1$match), by = 1)) +
  theme(text = element_text(family = font),
        legend.position = "top",
        plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        plot.subtitle = ggtext::element_textbox(size = 15, hjust = 0.5),
        plot.caption = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text = element_text(face = "bold"),
        axis.text.y = ggtext::element_markdown()))

title <- team_name |> str_to_lower() |> str_replace(" ", "_")

##save plot
ggsave(
  plot = p,
  filename = here::here("output", glue::glue("{title}_squad_usage.jpg")),
  dpi = 300,
  height = 8,
  width = 12
)

##Load plot and league logos
plot <-
  here::here("output", glue::glue("{title}_squad_usage.jpg"))
logo <- readr::read_csv(file = here::here("output/team_logos/league_one_2022_2023_logos.csv")) |> 
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
magick::image_write(all_plot, path = here::here("output", glue::glue("{title}_squad_usage_full.jpg")))



