# Load packages -----------------------------------------------------------
pacman::p_load(worldfootballR, tidyverse, glue, here, gt, gtExtras)

# Load globals ------------------------------------------------------------
source(here::here("globals.R"))


# retrieving data for pulling logos ---------------------------------------
##select league - example for EFL League One
league <- worldfootballR::fb_league_urls(
  country = "ENG",
  gender = "M",
  season_end_year = 2024,
  tier = "3rd"
)

##team links from league
teams <-
  worldfootballR::fb_teams_urls(league)

##function to retrieve team logos (do once and save to .csv to avoid regularly scraping)
get_logos <- function(team_url, time_pause = 3) {
  Sys.sleep(time_pause)
  
  logo_temp <-
    team_url |>
    rvest::read_html() |>
    rvest::html_nodes(".teamlogo") |>
    rvest::html_attr("src")
  
  url <- team_url
  
  # Regex pattern to capture the team name
  pattern <- "/([^/]+)-Stats"
  
  # Extract the team name using str_extract()
  team_name <- str_extract(url, pattern)
  
  # Remove the forward slash and -Stats
  team_name <- sub("/(.+)-Stats", "\\1", team_name)
  
  d <-
    tibble(team = team_name,
           logo_url = logo_temp)
  
}

##apply to each team
logos <-
  purrr::map_dfr(teams, ~ get_logos(.x), .progress = TRUE) |>
  mutate(team = str_replace_all(team, "-", " "))

##save logos
# write.csv(x = logos, file = here::here("src", "team_logos", "league_one_2023_2024_logos.csv"))


# if already have logos saved ---------------------------------------------
##if logos are already saved, load from file
logos <-
  read.csv(file = here::here("src", "team_logos", "league_one_2023_2024_logos.csv"))

##pull league results & filter played matches
results <- worldfootballR::fb_match_results(
  country = "ENG",
  gender = "M",
  season_end_year = 2024,
  tier = "3rd"
) |>
  filter(!is.na(HomeGoals) & !is.na(AwayGoals)) |>
  mutate(Wk = as.numeric(Wk)) |>
  arrange(-Wk)

##select match week
week <- 6

##total season minutes
season_mins <- week * 90

##filter the results by week
resultos <-
  results |>
  filter(Wk == week)

##get match line ups and edit strings
data <- worldfootballR::fb_match_lineups(resultos$MatchURL) |>
  separate(col = "Age",
           into = c("age", "days"),
           sep = "-") |>
  filter(age <= 21) |>
  mutate(Starting = ifelse(Starting == "Pitch", "Played", "Came on as a sub and played")) |>
  mutate(
    name_age = glue::glue(
      "**{Player_Name}({age})**<br>"
    ),
    info_string = glue::glue(
      "{name_age} {Starting} {Min}minutes for {Team}"
    ),
    goal_string = ifelse(
      Gls > 0 | Ast > 0,
      glue::glue("{info_string} getting **{Gls} goal/s // {Ast} Assist/s**"),
      glue::glue("{info_string}")
    ),
    final_string = ifelse(
      Ast == 0 & Gls == 0,
      glue::glue("{info_string}"),
      glue::glue("{goal_string}")
    )
  ) |>
  arrange(Team) |>
  rename(team = "Team") |>
  left_join(logos, by = "team") |>
  mutate(logo_url = link_to_img(logo_url))

##number of players that meet the criteria to split tables
num_players <- nrow(data)
half_num_players <- ceiling(num_players / 2)

##pull player links for total season minutes
player_links <- data$PlayerURL

##retrieve player season details and filter by season and league
player_details <-
  worldfootballR::fb_player_season_stats(player_url = player_links, stat_type = "standard") |>
  filter(Season == "2023-2024",
         Comp == "3. League One")

##join combine all data for plotting table
final_data <-
  left_join(data |> janitor::clean_names(),
            player_details |> select(player_name, Min_Time)) |>
  mutate(
    player_perc =  round(Min_Time / season_mins * 100, 2),
    final_string = glue::glue(
      "{final_string}<br>*Season Minutes: {Min_Time}/{season_mins} // {player_perc}%*"
    )
  )


# Plot tables -------------------------------------------------------------
##first table
tab_1 <-
  final_data |>
  slice(1:half_num_players) |>
  select(logo_url, final_string) |>
  gt::gt() |>
  gt::cols_label(logo_url = "",
                 final_string = "") |>
  gt::fmt_markdown(columns = everything()) |>
  gtExtras::gt_theme_538() |>
  gt::cols_align(align = "left") |>
  gt::tab_header(
    title = "League One",
    subtitle = glue::glue("Players involved in Matchday {week} who are 21 or under")
  ) |>
  gt::tab_source_note(gt::md("**Viz**: @AnalyticsOxford<br>**Data**: Fbref.com"))

##second table
tab_2 <-
  final_data |>
  slice(half_num_players + 1:num_players) |>
  select(logo_url, final_string) |>
  gt::gt() |>
  gt::cols_label(logo_url = "",
                 final_string = "") |>
  gt::cols_align(align = "left") |>
  gt::fmt_markdown(columns = everything()) |>
  gtExtras::gt_theme_538() |>
  gt::tab_header(title = gt::md("<br><br>"))

##plot 2 tables together
gtExtras::gt_two_column_layout(tables = list(tab_1, tab_2))

##save
gtExtras::gt_two_column_layout(
  tables = list(tab_1, tab_2),
  output = "save",
  path = here::here("R", "weekly_young_players"),
  filename = glue::glue("league_one_week_{week}.png"),
  vwidth = 1500
)
