# Load Packages -----------------------------------------------------------
library(tidyverse)
library(readxl) ##to read files from wyscout
library(gt) ##for table
library(gtExtras) ##for percentile plot & table theming

##path of selected .xlsx file
file_path <- "/Users/markjwilkins/Downloads/cm_test.xlsx"

##read file and clean headers before replacing NA values with 0
df <- readxl::read_xlsx(path = file_path) %>%
  janitor::clean_names() %>%
  mutate(across(.cols = c(7:38), .fns = ~ replace_na(., 0))) ##select columns to apply replace_na function

##function to normalise data
normalized <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

##select minimum player minutes
mins <- 500

##filter data, reshape and normalise selected columns
df_filt <-
  df %>%
  filter(minutes_played >= mins) %>% ##filter
  mutate(across(.cols = c(7:38), .fns = normalized)) %>% ##normalise
  select(1, 7:38) %>% ##select columns
  pivot_longer(cols = c(2:33),
               names_to = "stat") %>% ##reshape
  mutate(value = round(value, digits = 2)) 

##how may players in filetred data?
n_players <- length(unique(df_filt$player))

##player details for viz
player_name <- "O. Arblaster"
team <- "Port Vale"
league <- "League One"
season <- "2023/2024"
position <- "Centre Midfielders"

##create categories for metrics
creative <-
  c(
    "dribbles_per_90",
    "successful_dribbles_percent",
    "deep_completions_per_90",
    "smart_passes_per_90",
    "accurate_smart_passes_percent",
    "through_passes_per_90",
    "accurate_through_passes_percent",
    "key_passes_per_90",
    "passes_to_penalty_area_per_90",
    "accurate_passes_to_penalty_area_percent",
    "x_g_per_90",
    "progressive_runs_per_90",
    "successful_attacking_actions_per_90"
  )

defensive <-
  c(
    "aerial_duels_per_90",
    "aerial_duels_won_percent",
    "defensive_duels_per_90",
    "defensive_duels_won_percent",
    "successful_defensive_actions_per_90",
    "fouls_per_90",
    "p_adj_interceptions",
    "offensive_duels_per_90",
    "offensive_duels_won_percent"
  )

passing <-
  c(
    "passes_per_90",
    "accurate_passes_percent",
    "forward_passes_per_90",
    "accurate_forward_passes_percent",
    "short_medium_passes_per_90",
    "average_pass_length_m",
    "average_long_pass_length_m",
    "progressive_passes_per_90",
    "accurate_progressive_passes_percent"
  )

##classify metrics and clean up strings
final_df <-
  df_filt %>%
  filter(player == player_name) %>% 
  mutate(
    spec = case_when(
      stat %in% creative ~ "creative",
      stat %in% defensive ~ "defensive",
      stat %in% passing ~ "passing"
    )
  ) %>%
  filter(!is.na(spec)) %>% 
  mutate(
    stat = str_remove_all(stat, "_per_90$"),
    stat = str_replace_all(stat, "_", " "),
    stat = str_to_title(stat),
    stat = str_replace_all(stat, "Percent$", "%"),
    stat = str_replace_all(stat, "M$", "(M)"),
    stat = case_when(
      stat == "X G" ~ "xG",
      stat == "P Adj Interceptions" ~ "Possession Adjusted Interceptions",
      TRUE ~ stat
    )
  )


# PLOT! -------------------------------------------------------------------
(p <-
   final_df %>%
   group_by(spec) %>%
   select(-player) %>% 
   gt::gt() %>%
   gtExtras::gt_theme_538() %>%
   gt_plt_percentile(value, width = 100, scale = 100) %>%
   gt::cols_label(.list = c(stat = "",
                            value = "")) %>%
   tab_style(
     style = cell_text(color = "black", weight = "bold"),
     locations = list(cells_row_groups(),
                      cells_column_labels(everything()))
   ) %>%
   gt::tab_header(
     title = md(glue::glue("**{player_name} · {team}**")),
     subtitle = glue::glue(
       "{league} · {season} · Compared to {n_players} {position} with over {mins}minutes"
     )
   ) %>%
   tab_source_note(md("**Table**: @AnalyticsOxford | **Data**: Wyscout")) %>%
   tab_options(
     row_group.border.top.width = px(4),
     row_group.border.top.color = "black",
     row_group.border.bottom.color = "black"
   ))

##tidy string for save
player_to_save <- str_replace_all(string = player_name, pattern = ". ", "_") %>% str_to_lower()

##save plot
gt::gtsave(data = p,
           filename = glue::glue("{player_to_save}_overview.png"), 
           path = here::here("R", "player_percentile"))
