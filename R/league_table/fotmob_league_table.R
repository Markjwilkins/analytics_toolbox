
# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, jsonlite, gt, gtExtras, MetBrewer)

# Data --------------------------------------------------------------------
##set table colours - relegation etc
cols <-
  MetBrewer::met.brewer(palette_name = "Hiroshige", direction = -1)

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

##find all matches that have been played
matches <-
  league_json$matches$data$allMatches |> filter(status$finished == TRUE)

##pull match information
match_info <-
  worldfootballR::fotmob_get_match_info(match_ids = unique(matches$id))

##league and season for viz titles
league <- unique(match_info$league_name)
season <- unique(match_info$parent_league_season)

##current date
current_date <- format(Sys.Date(), format = "%d/%m/%Y")

##create league table
league_table <-
  worldfootballR::fotmob_get_league_tables(league_id = league_id) |>
  filter(table_idx == 1:24) |>
  slice(1:24)

##function to construct logo image path
logo_image <- function(team_id, width = 20) {
  glue::glue("https://images.fotmob.com/image_resources/logo/teamlogo/{team_id}.png")
}

##small data wrangling
data <-
  league_table |>
  mutate(image_link = logo_image(team_id = unique(league_table$table_id))) |>
  separate(table_scores_str, into = c("goals_for", "goals_against")) |>
  mutate(ppg = round(table_pts / table_played, digits = 2))

##could use colours from data$table_qual_color - instead using some colours from MetBrewer
colours <- tibble(top = cols[1],
                  po = cols[4],
                  rel = cols[10])

##PLOT TABLE!
(
  tab <-
    data %>%
    ##select columns
    select(table_idx, image_link, table_name, 10:17, 21) |>
    gt::gt()  |>
    ##logos
    gtExtras::gt_img_rows(column = image_link, height = 20) |>
    ##change column names
    gt::cols_label(
      table_idx = "Rank",
      image_link = "",
      table_name = "Team",
      table_played = "Played",
      table_wins = "W",
      table_draws = "D",
      table_losses = "L",
      goals_for = "GF",
      goals_against = "GA",
      table_goal_con_diff = "GD",
      table_pts = "Points"
    )  |>
    ##apply 538 theme
    gtExtras::gt_theme_538()  |>
    ##highlight rows for top 4/5/and bottom 3
    gtExtras::gt_highlight_rows(
      columns = everything(),
      rows = 1:2,
      fill = colours$top,
      font_weight = "normal"
    )  |>
    gtExtras::gt_highlight_rows(
      columns = everything(),
      rows = 3:6,
      fill = colours$po,
      font_weight = "normal"
    )  |>
    gtExtras::gt_highlight_rows(
      columns = everything(),
      rows = 21:24,
      fill = colours$rel,
      font_weight = "normal"
    )  |>
    ##align text
    gt::cols_align("center")  |>
    ##format title and subtitle (including league logo)
    gt::tab_header(
      title = gt::md(
        glue::glue(
          "<img src='{league_logo}' style='height:60px;'><br>{league} - League Table"
        )
      ),
      subtitle = gt::md(glue::glue("{season} // Up to: **{current_date}**"))
    )  |>
    ##add data and tag
    gt::tab_source_note(gt::md(
      "**Viz**: @AnalyticsOxford<br>**Data**: Fotmob.com"
    ))
)

##adjust strings for save title
league_title <- league |> str_to_lower() |> str_replace(" ", "_")
season_title <- season |> str_replace("/", "_")

##save gt table
gt::gtsave(tab, here::here(
  "R",
  "league_table",
  glue::glue("{league_title}_{season_title}_league_table.png")
), expand = 60)
